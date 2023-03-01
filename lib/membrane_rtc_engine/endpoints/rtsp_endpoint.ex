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
    An Endpoint responsible for receiving an RTSP stream and sending the appropriate
    media track to other Endpoints (see **Limitations**).

    This module requires the following plugins to be present in your `mix.exs`:
    ```
    [
      :connection,
      :membrane_rtsp,
      :membrane_udp_plugin
    ]
    ```

    ## Limitations
    Currently, only H264 streams are supported.

    At the moment, if H264 parameters are passed out-of-band, only the HLS Endpoint
    will be able subscribe to tracks created by the RTSP Endpoint.
    """

    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine.Endpoint.RTSP.ConnectionManager
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
    alias Membrane.RTC.Engine.Track

    # FIXME this should not be hardcoded
    @rtp_port 20_000

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
                ]

    @impl true
    def handle_init(_ctx, opts) do
      state = %{
        rtc_engine: opts.rtc_engine,
        source_uri: opts.source_uri,
        stream_id: nil,
        track_data: nil,
        video: nil,
        ssrc: nil
      }

      structure = [
        child(:udp_source, %Membrane.UDP.Source{
          local_port_no: @rtp_port,
          recv_buffer_size: 500_000
        })
        |> via_in(Pad.ref(:rtp_input, make_ref()))
        |> child(:rtp, Membrane.RTP.SessionBin)
      ]

      {[spec: structure], state}
    end

    @impl true
    def handle_spec_started([:udp_source, :rtp], _ctx, state) do
      connection_manager_spec = [
        %{
          id: "ConnectionManager",
          start:
            {ConnectionManager, :start_link,
             [
               [
                 stream_uri: state.source_uri,
                 port: @rtp_port,
                 endpoint: self()
               ]
             ]},
          restart: :transient
        }
      ]

      # XXX we should probably add to an existing supervision tree instead
      Supervisor.start_link(connection_manager_spec,
        strategy: :one_for_one,
        name: Membrane.RTC.Engine.Endpoint.RTSP.Supervisor
      )

      {[], state}
    end

    @impl true
    def handle_spec_started(_children, _ctx, state) do
      {[], state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:output, {track_id, :high}) = pad, _ctx, state) do
      {^track_id, track} = state.track_data

      structure = [
        get_child(:rtp)
        |> via_out(Pad.ref(:output, state.ssrc), options: [depayloader: nil])
        |> via_in(Pad.ref(:input, {track_id, :high}))
        |> child(
          {:track_sender, track_id},
          %TrackSender{
            track: track,
            is_keyframe_fun: fn buf, :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buf.payload, :idr) end
          },
          get_if_exists: true
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
    def handle_child_notification({:new_rtp_stream, _ssrc, _fmt, _extensions} = msg, :rtp, _ctx, state)
        when state.stream_id != nil do
      raise("Received unexpected, second RTP stream: #{inspect(msg)}")
    end

    @impl true
    def handle_child_notification({:new_rtp_stream, ssrc, 96, _extensions} = msg, :rtp, ctx, state) do
      Membrane.Logger.debug("New RTP stream (H264) connected: #{inspect(msg)}")

      {:endpoint, endpoint_id} = ctx.name
      stream_id = Track.stream_id()
      track = Track.new(:video, stream_id, endpoint_id, :H264, state.video.rtpmap.clock_rate, state.video.fmtp)

      actions = [
        notify_parent: {:publish, {:new_tracks, [track]}},
        notify_parent: {:track_ready, track.id, :high, :H264}
      ]

      {actions, %{state | stream_id: stream_id, track_data: {track.id, track}, ssrc: ssrc}}
    end

    @impl true
    def handle_child_notification({:new_rtp_stream, _ssrc, _fmt, _extensions} = msg, :rtp, _ctx, _state) do
      raise("Unsupported RTP stream connected: #{inspect(msg)}")
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
    def handle_info({:rtsp_setup_complete, options}, _ctx, state) do
      Membrane.Logger.debug("Endpoint received source options: #{inspect(options)}")

      {:ok, fmtp} = ExSDP.Attribute.FMTP.parse(options[:fmtp])

      state = %{
        state
        | video: %{
            fmtp: fmtp,
            rtpmap: options[:rtpmap]
          }
      }

      {[], state}
    end

    @impl true
    def handle_info(info, _ctx, state) do
      Membrane.Logger.warn("Unexpected info: #{inspect(info)}. Ignoring.")
      {[], state}
    end
  end
end
