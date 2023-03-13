if Enum.all?(
     [
       Connection,
       Membrane.RTSP,
       Membrane.UDP.Source
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.RTSP do
    @moduledoc """
    An Endpoint responsible for connecting to a remote RTSP stream source
    and sending the appropriate media track to other Endpoints (see **Limitations**).

    This module requires the following plugins to be present in your `mix.exs`:
    ```
    [
      {:connection, "~> 1.1"},
      {:membrane_rtsp, "0.3.0"},
      {:membrane_udp_plugin, "~> 0.9.1"}
    ]
    ```

    ## Limitations
    Currently, only H264 streams are supported.

    At the moment, if H264 parameters are passed out-of-band, only the HLS Endpoint
    will be able to subscribe to tracks created by the RTSP Endpoint.
    """

    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine.Endpoint.RTSP.ConnectionManager
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
    alias Membrane.RTC.Engine.Track

    @required_deps [
      Connection,
      Membrane.RTSP,
      Membrane.UDP.Source
    ]

    @spec get_required_deps() :: list(atom())
    def get_required_deps(), do: @required_deps

    def_output_pad :output,
      demand_unit: :buffers,
      accepted_format: Membrane.RTP,
      availability: :on_request

    def_options rtc_engine: [
                  spec: pid(),
                  description: "PID of parent Engine"
                ],
                source_uri: [
                  spec: URI.t(),
                  description: "URI of source stream"
                ],
                rtp_port: [
                  spec: pos_integer() | nil,
                  description:
                    "Port to receive RTP stream at. If not provided, will pick any available one",
                  default: nil
                ]

    @impl true
    def handle_init(_ctx, opts) do
      state = %{
        rtc_engine: opts.rtc_engine,
        source_uri: opts.source_uri,
        track: nil,
        stream_params: nil
      }

      structure = [
        child(:udp_source, %Membrane.UDP.Source{
          # Pick any port if not specified
          local_port_no: opts.rtp_port || 0
        })
        |> via_in(Pad.ref(:rtp_input, make_ref()))
        |> child(:rtp, Membrane.RTP.SessionBin)
      ]

      {[spec: structure], state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:output, {track_id, :high}) = pad, _ctx, state)
        when track_id == state.track.id do
      Membrane.Logger.debug("Pad added for track #{inspect(track_id)}, variant :high")

      structure = [
        get_child(:rtp)
        |> via_out(Pad.ref(:output, state.stream_params.ssrc), options: [depayloader: nil])
        |> via_in(Pad.ref(:input, {track_id, :high}))
        |> child(
          {:track_sender, track_id},
          %TrackSender{
            track: state.track,
            is_keyframe_fun: fn buf, :H264 ->
              Membrane.RTP.H264.Utils.is_keyframe(buf.payload, :idr)
            end
          }
        )
        |> via_out(pad)
        |> bin_output(pad)
      ]

      {[spec: structure], state}
    end

    @impl true
    def handle_pad_removed(Pad.ref(:output, {_track_id, _variant}), _ctx, state) do
      {[], state}
    end

    @impl true
    def handle_parent_notification({:new_tracks, _tracks}, _ctx, state) do
      {[], state}
    end

    @impl true
    def handle_parent_notification({:remove_tracks, _tracks}, _ctx, state) do
      {[], state}
    end

    @impl true
    def handle_parent_notification(msg, _ctx, state) do
      Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
      {[], state}
    end

    @impl true
    def handle_child_notification({:connection_info, _address, port}, :udp_source, _ctx, state) do
      connection_manager_spec = [
        %{
          id: "ConnectionManager",
          start:
            {ConnectionManager, :start_link,
             [
               [
                 stream_uri: state.source_uri,
                 port: port,
                 endpoint: self()
               ]
             ]},
          restart: :transient
        }
      ]

      Supervisor.start_link(connection_manager_spec,
        strategy: :one_for_one,
        name: Membrane.RTC.Engine.Endpoint.RTSP.Supervisor
      )

      {[], state}
    end

    @impl true
    def handle_child_notification(
          {:new_rtp_stream, ssrc, fmt, _extensions} = msg,
          :rtp,
          _ctx,
          state
        )
        when is_nil(state.stream_params) do
      Membrane.Logger.debug("New RTP stream connected: #{inspect(msg)}")

      state = %{state | stream_params: %{ssrc: ssrc, fmt: fmt}}

      if is_nil(state.track) do
        Membrane.Logger.debug("Track will be created once RTSP setup completes")
        {[], state}
      else
        {ready_track_actions(state), state}
      end
    end

    @impl true
    def handle_child_notification(
          {:new_rtp_stream, _ssrc, _fmt, _extensions} = msg,
          :rtp,
          _ctx,
          _state
        ) do
      raise("Received unexpected, second RTP stream: #{inspect(msg)}")
    end

    @impl true
    def handle_child_notification({:estimation, _data}, {:track_sender, _track_id}, _ctx, state) do
      {[], state}
    end

    @impl true
    def handle_child_notification(notification, element, _ctx, state) do
      Membrane.Logger.warn(
        "Unexpected notification from `#{inspect(element)}`: #{inspect(notification)}. Ignoring."
      )

      {[], state}
    end

    @impl true
    def handle_info({:rtsp_setup_complete, options}, ctx, state) do
      Membrane.Logger.debug("Endpoint received source options: #{inspect(options)}")

      {:ok, fmtp} = ExSDP.Attribute.FMTP.parse(options[:fmtp])
      rtpmap = options[:rtpmap]

      encoding = rtpmap.encoding |> String.to_atom()

      if encoding != :H264 do
        raise("RTSP setup returned unsupported stream encoding: #{inspect(encoding)}")
      end

      {:endpoint, endpoint_id} = ctx.name

      track =
        Track.new(
          :video,
          Track.stream_id(),
          endpoint_id,
          encoding,
          rtpmap.clock_rate,
          fmtp,
          ctx: %{rtpmap: rtpmap}
        )

      Membrane.Logger.debug("Publishing new WebRTC track: #{inspect(track)}")
      actions = [notify_parent: {:publish, {:new_tracks, [track]}}]
      state = %{state | track: track}

      if is_nil(state.stream_params) do
        Membrane.Logger.debug(
          "Track will be marked as ready once an RTP stream has been received"
        )

        {actions ++ create_nat_binding_actions(state, options[:server_port]), state}
      else
        {actions ++ ready_track_actions(state), state}
      end
    end

    @impl true
    def handle_info(info, _ctx, state) do
      Membrane.Logger.warn("Unexpected info: #{inspect(info)}. Ignoring.")
      {[], state}
    end

    defp ready_track_actions(state) do
      if state.track.ctx.rtpmap.payload_type != state.stream_params.fmt do
        raise("Payload type mismatch between RTP mapping and received stream")
      end

      [notify_parent: {:track_ready, state.track.id, :high, state.track.encoding}]
    end

    defp create_nat_binding_actions(state, server_port) do
      Membrane.Logger.debug(
        "Probing server port #{inspect(server_port)} to attempt to create NAT binding..."
      )

      # FIXME: The following assumes IPv4, which may not always be the case
      # Also, this is a bit too low-level for the endpoint,
      # but there really is no other element which could handle it at the moment...
      {:ok, address} =
        URI.parse(state.source_uri)
        |> Map.get(:host)
        |> to_charlist()
        |> :inet.getaddr(:inet)

      [notify_child: {:udp_source, {:probe, address, server_port}}]
    end
  end
end
