defmodule Membrane.RTC.Engine.Endpoint.HLS.Switch do
  @moduledoc false
  # Module responsible for switching between multiple tracks
  # useful when only one hls output is needed (for instance, when resource-heavy transcoding is used)
  # creates single pipelines for the audio and video
  # endpoint, from which tracks are used, can be changed via message to the element

  use Membrane.Filter

  alias Membrane.RemoteStream
  alias Membrane.RTC.Engine.Endpoint.HLS.Utils

  def_input_pad :input,
                demand_mode: :auto,
                availability: :on_request,
                caps: {RemoteStream, type: :packetized, content_format: one_of([
                    Membrane.H264,
                    Membrane.AAC,
                    Membrane.Opus
                  ])},
                options: [
                  track: [
                    spec: Membrane.RTC.Engine.Track.t(),
                    description: """
                    Track metadata.
                    """
                  ]
                ]

  def_output_pad :output,
                 demand_mode: :auto,
                 availability: :on_request,
                 caps: {RemoteStream, type: :packetized, content_format: one_of([
                   Membrane.H264,
                   Membrane.AAC,
                   Membrane.Opus
                 ])}

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
    IO.inspect(origin, label: :HANDLED_MESSAGE_FROM_BIN)
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
      {{:ok, buffer: {Pad.ref(:output, track_type), buffer}}, state}
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
      {{:ok, caps: {Pad.ref(:output, track_type), caps}}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id), ctx, state) do
    state = put_in(state, [:tracks, track_id], ctx.options.track)

    # state = if state.cur_origin == nil do
    #   track = Map.fetch!(state.tracks, track_id)
    #   Map.put(state, :cur_origin, track.origin)
    # else
    #   state
    # end

    {:ok, state}
  end

  @impl true
  def handle_pad_added(_pad, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, track_id), _ctx, state) do
    track = Map.fetch!(state.tracks, track_id)
    track_type = Utils.get_track_type(track)
    # TODO co kiedy input_pad nie jest current origin padem?
    {{:ok, end_of_stream: Pad.ref(:output, track_type)}, state}
  end
end
