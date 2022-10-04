defmodule Membrane.RTC.Engine.Endpoint.HLS.Switch do
  @moduledoc false
  # Module responsible for switching between multiple tracks
  # useful when only one hls output is needed (for instance, when resource-heavy transcoding is used)
  # creates individual pipelines for the audio and video
  # endpoint, from which tracks are used, can be changed via message to the element

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.RemoteStream

  def_input_pad :input,
                demand_mode: :auto,
                availability: :on_request,
                caps: [
                  Membrane.AAC,
                  Membrane.Opus,
                  Membrane.H264,
                  {RemoteStream, type: :packetized, content_format: one_of([
                    Membrane.H264,
                    Membrane.AAC,
                    Membrane.Opus
                  ])}],
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
                 caps: [
                  Membrane.AAC,
                  Membrane.Opus,
                  Membrane.H264,
                  {RemoteStream, type: :packetized, content_format: one_of([
                    Membrane.H264,
                    Membrane.AAC,
                    Membrane.Opus
                  ])}]

  @impl true
  def handle_init(_opts) do
    state = %{
      tracks: %{},
      tracks_caps: %{},
      awaiting_id: :static,
      cur_id: nil,
      awaiting_origin: nil,
      universal_pts: nil,
      prev_pts: %{},
      prev_diff: nil,
    }

    {:ok, state}
  end

  @impl true
  def handle_other({:change_origin, origin}, _ctx, state) do
    awaiting_id = case Enum.find(state.tracks, fn {_track_id, track} -> track.origin == origin end) do
      nil -> nil
      {_track_id, track} -> track.id
    end

    state =
      state
      |> Map.put(:awaiting_origin, origin)
      |> Map.put(:awaiting_id, awaiting_id)

    {:ok, state}
  end

  @impl true
  def handle_other(msg, _ctx, state) do
    Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
    {:ok, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, track_id), buffer, _ctx, %{awaiting_id: track_id} = state)
  when buffer.metadata.is_keyframe and not is_nil(state.prev_diff) do
    {new_diff, state} = update_pts(track_id, state, buffer)
    state = %{state | universal_pts: state.universal_pts + new_diff}
    buffer = %{buffer | pts: state.universal_pts}

    state =
      state
      |> Map.put(:cur_id, state.awaiting_id)
      |> Map.put(:awaiting_id, nil)
      |> Map.put(:awaiting_origin, nil)

    caps_action = [caps: {Pad.ref(:output), state.tracks_caps[track_id]}]
    buffer_action = [buffer: {Pad.ref(:output), buffer}]

    {{:ok, caps_action ++ buffer_action}, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, track_id), buffer, _ctx, %{cur_id: track_id} = state) do
    {new_diff, state} = update_pts(track_id, state, buffer)
    state = %{state | universal_pts: state.universal_pts + new_diff}
    buffer = %{buffer | pts: state.universal_pts}

    {{:ok, buffer: {Pad.ref(:output), buffer}}, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, track_id), buffer, _ctx, state) do
    {new_diff, state} = update_pts(track_id, state, buffer) # TODO do przetestowania

    {:ok, state}
  end

  @impl true
  def handle_caps(Pad.ref(:input, track_id), caps, _ctx, state) do
    state = put_in(state, [:tracks_caps, track_id], caps)

    actions = if track_id == state.cur_id do
      [caps: {Pad.ref(:output), caps}]
    else
      []
    end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, :static), _ctx, state) do
    # Switch expects to be liked to source of static video/audio
    # which will be forwarded when end_of_stream arrives at currently forwarded track's pad
    {:ok, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id), ctx, state) do
    track = ctx.options.track
    state = put_in(state, [:tracks, track_id], track)
    state = if state.awaiting_origin == track.origin do
      %{state | awaiting_id: track_id}
    else
      state
    end

    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, track_id), _ctx, state) do
    {_value, state} = pop_in(state, [:tracks, track_id])
    {_value, state} = pop_in(state, [:tracks_caps, track_id])

    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, track_id), _ctx, %{cur_id: track_id} = state) do
    awaiting_id = if is_nil(state.awaiting_id), do: :static, else: state.awaiting_id
    {:ok, %{state | awaiting_id: awaiting_id}}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, _track_id), _ctx, state), do: {:ok, state}

  defp update_pts(track_id, state, buffer) do
    state = if is_nil(state.universal_pts), do: %{state | universal_pts: buffer.pts}, else: state

    new_diff = if Map.has_key?(state.prev_pts, track_id) do
      buffer.pts - state.prev_pts[track_id]
    else
      state.prev_diff
    end

    state =
      state
      |> Map.put(:prev_diff, new_diff)
      |> put_in([:prev_pts, track_id], buffer.pts)

    {new_diff, state}
  end
end
