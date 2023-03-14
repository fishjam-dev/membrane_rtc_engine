if Enum.all?(
     Membrane.RTC.Engine.MixProject.rtsp_endpoint_deps(),
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

    If a connection drops .. TODO write the rest of me
    Run only on server without NAT .. TODO write the rest of me
    """

    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine.Endpoint.RTSP.ConnectionManager
    alias Membrane.RTC.Engine.Endpoint.RTSP.PortAllocator
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
    alias Membrane.RTC.Engine.Track

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
                port_range: [
                  spec: {pos_integer(), pos_integer()},
                  description:
                    "Port range from which to pick a port to receive RTP stream at",
                  default: {62_137, 62_420}
                ]

    @impl true
    def handle_init(_ctx, opts) do
      rtp_port = PortAllocator.get_port(opts.rtc_engine)

      state = %{
        rtc_engine: opts.rtc_engine,
        source_uri: opts.source_uri,
        rtp_port: rtp_port,
        track: nil,
        ssrc: nil
      }

      # This UDP.Source is used only to reserve a port
      # It will be replaced once we receive negotiated RTP mapping
      # Currently, there is no other way to do this, since UDP.Source
      # has a static output pad which may only be linked once
      structure = [
        child(:temporary_udp_source, %Membrane.UDP.Source{
          local_port_no: rtp_port
        })
        |> child(:fake_sink, Membrane.Fake.Sink.Buffers)
      ]

      {[spec: structure], state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:output, {track_id, :high}) = pad, _ctx, state)
        when track_id == state.track.id do
      Membrane.Logger.debug("Pad added for track #{inspect(track_id)}, variant :high")

      structure = [
        get_child(:rtp)
        |> via_out(Pad.ref(:output, state.ssrc), options: [depayloader: nil])
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
    def handle_child_notification(
          {:connection_info, _address, port},
          :temporary_udp_source,
          _ctx,
          state
        ) do
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

      {[], %{state | rtp_port: port}}
    end

    @impl true
    def handle_child_notification(
          {:new_rtp_stream, ssrc, fmt, _extensions} = msg,
          :rtp,
          _ctx,
          state
        )
        when is_nil(state.ssrc) do
      Membrane.Logger.debug("New RTP stream connected: #{inspect(msg)}")

      state = %{state | ssrc: ssrc}

      if state.track.ctx.rtpmap.payload_type != fmt do
        raise("Payload type mismatch between RTP mapping and received stream")
      end

      {[notify_parent: {:track_ready, state.track.id, :high, state.track.encoding}], state}
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

      structure = [
        child(:udp_source, %Membrane.UDP.Source{
          local_port_no: state.rtp_port
        })
        |> via_in(Pad.ref(:rtp_input, make_ref()))
        |> child(:rtp, %Membrane.RTP.SessionBin{
          fmt_mapping: %{rtpmap.payload_type => {encoding, rtpmap.clock_rate}}
        })
      ]

      actions = [
        remove_child: [:temporary_udp_source, :fake_sink],
        spec: structure,
        notify_parent: {:publish, {:new_tracks, [track]}}
      ]

      state = %{state | track: track}

      {actions ++ create_nat_binding_actions(state, options[:server_port]), state}
    end

    @impl true
    def handle_info(info, _ctx, state) do
      Membrane.Logger.warn("Unexpected info: #{inspect(info)}. Ignoring.")
      {[], state}
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

      [notify_child: {:udp_source, {:pierce_nat, address, server_port}}]
    end
  end
end
