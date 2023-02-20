if Enum.all?(
     [
       Membrane.RTSP,
       Membrane.UDP
     ],
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.RTSP do
    # FIXME write docs
    @moduledoc false

    use Membrane.Bin

    require Membrane.Logger

    alias Membrane.RTC.Engine
    alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender
    alias Membrane.RTC.Engine.Track

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
        track: nil
      }

      structure = []

      {[spec: structure], state}
    end

    @impl true
    def handle_pad_added(Pad.ref(:output, {_track_id, _variant}) = _pad, _ctx, state) do
      %Track{encoding: encoding} = track = state.track

      structure = []

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
    def handle_child_notification({:new_rtp_stream, _ssrc, _fmt, _extensions}, :rtp, _ctx, state)
        when state.track == nil do
      Membrane.Logger.debug(":new_rtp_stream")
      track = Track.new(:video, Track.stream_id(), nil, nil, nil, nil)

      actions = [
        notify_parent: {:publish, {:new_tracks, [track]}},
        notify_parent: {:track_ready, nil, nil, nil}
      ]

      {actions, %{state | track: track}}
    end

    @impl true
    def handle_child_notification({:new_rtp_stream, _ssrc, _fmt, _extensions}, :rtp, _ctx, state) do
      raise(":new_rtp_stream received with track already present")
    end

    @impl true
    def handle_child_notification(notification, element, _context, state) do
      Membrane.Logger.warn(
        "Unexpected notification from `#{inspect(element)}`: #{inspect(notification)}. Ignoring."
      )

      {[], state}
    end

    @impl true
    def handle_info(info, _ctx, state) do
      Membrane.Logger.warn("Unexpected info: #{inspect(info)}. Ignoring.")
      {[], state}
    end
  end
end
