defmodule Membrane.RTC.Engine.Endpoint.HLS.Switch do
  @moduledoc false
  # Module responsible for switching between multiple tracks between rtc_engine and hls_endpoint
  # useful when only one hls output is needed (for instance, when resource-heavy transcoding is used)
  # creates single pipelines for the audio and video
  # endpoint, from which tracks are used, can be changed via message to the element

  use Membrane.Filter

  alias Membrane.RTC.Engine.Endpoint.HLS.Utils

  def_input_pad :input,
                demand_mode: :auto,
                availability: :on_request,
                caps: [Membrane.H264, Membrane.AAC, Membrane.Opus],
                options: [
                  track: [
                    spec: Membrane.RTC.Engine.Track.t(),
                    description: """
                    Track metadata.
                    """
                  ]
                ]

  def_output_pad :video,
                 demand_mode: :auto,
                 caps: [Membrane.H264]

  def_output_pad :audio,
                 demand_mode: :auto,
                 caps: [Membrane.AAC, Membrane.Opus]

  @impl true
  def handle_init(_opts) do
    state = %{
      tracks: %{},
      cur_origin: nil,
      caps_sent: %{video: false, audio: false},
    }

    {:ok, state}
  end

  @impl true
  def handle_other({:change_origin, origin}, _ctx, state) do
    # TODO check if track with origin are in the state.tracks
    state = Map.put(state, :cur_origin, origin)
    {:ok, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, track_id), buffer, _ctx, state) do
    track = Map.fetch!(state.tracks, track_id)
    current? = track.origin == state.cur_origin
    track_type = Utils.get_track_type(track)

    if current? do
      {{:ok, buffer: {track_type, buffer}}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_caps(Pad.ref(:input, track_id), caps, _ctx, state) do
    track = Map.fetch!(state.tracks, track_id)
    track_type = Utils.get_track_type(track)

    if not state.caps_sent[track_type] do
      state = put_in(state, [:caps_sent, track_type], true)
      {{:ok, caps: {track_type, caps}}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id), ctx, state) do
    state = put_in(state, [:tracks, track_id], ctx.options.track)

    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, track_id), _ctx, state) do
    track = Map.fetch!(state.tracks, track_id)
    track_type = Utils.get_track_type(track)
    # TODO co kiedy input_pad nie jest current origin padem?
    {{:ok, end_of_stream: track_type}, state}
  end
end
