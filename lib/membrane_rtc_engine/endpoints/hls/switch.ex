defmodule Membrane.RTC.Engine.Endpoint.HLS.Switch do
  @moduledoc false
  # Module responsible for switching between multiple tracks
  # useful when only one hls output is needed (for instance, when resource-heavy transcoding is used)
  # creates individual pipelines for the audio and video
  # endpoint, from which tracks are used, can be changed via message to the element

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.{RemoteStream, H264, AAC, Opus}

  def_input_pad :input,
                demand_mode: :auto,
                availability: :on_request,
                caps: [AAC, Opus,H264,
                  {RemoteStream, type: :packetized, content_format: one_of([H264, AAC, Opus])}
                ],
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
                 caps: [AAC, Opus,H264,
                 {RemoteStream, type: :packetized, content_format: one_of([H264, AAC, Opus])}
                 ]

  def_options type: [
                spec: :audio | :video,
                description: """
                Type of input and output tracks.
                """
              ]

  @impl true
  def handle_init(opts) do
    state = %{
      type: opts.type,
      tracks: %{},
      tracks_caps: %{},
      awaiting_id: :static,
      awaiting_origin: nil,
      cur_id: nil,
      universal_timestamp: nil,
      prev_timestamp: %{},
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
  when buffer.metadata.is_keyframe and (is_nil(state.universal_timestamp) or not is_nil(state.prev_diff)) do
    cur_timestamp = get_timestamp(buffer, state.type)

    {state, buffer} = if is_nil(state.universal_timestamp) do
      state = %{state | universal_timestamp: cur_timestamp}
      buffer = %Buffer{buffer | pts: state.universal_timestamp}
      {state, buffer}
    else
      new_diff = if Map.has_key?(state.prev_timestamp, track_id) do
        cur_timestamp - state.prev_timestamp[track_id]
      else
        state.prev_diff
      end

      state = %{state | prev_diff: new_diff}
      state = %{state | universal_timestamp: state.universal_timestamp + new_diff}
      buffer = %Buffer{buffer | pts: state.universal_timestamp}

      {state, buffer}
    end

    state = put_in(state, [:prev_timestamp, track_id], cur_timestamp)


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
    cur_timestamp = get_timestamp(buffer, state.type)

    {state, buffer} = if is_nil(state.universal_timestamp) do
      state = %{state | universal_timestamp: cur_timestamp}
      buffer = %Buffer{buffer | pts: state.universal_timestamp}
      {state, buffer}
    else
      new_diff = if Map.has_key?(state.prev_timestamp, track_id) do
        cur_timestamp - state.prev_timestamp[track_id]
      else
        state.prev_diff
      end

      state = %{state | prev_diff: new_diff}
      state = %{state | universal_timestamp: state.universal_timestamp + new_diff}
      buffer = %Buffer{buffer | pts: state.universal_timestamp}

      {state, buffer}
    end

    state = put_in(state, [:prev_timestamp, track_id], cur_timestamp)


    {{:ok, buffer: {Pad.ref(:output), buffer}}, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, track_id), buffer, _ctx, state) do
    cur_timestamp = get_timestamp(buffer, state.type)

    new_diff = if Map.has_key?(state.prev_timestamp, track_id) do
      cur_timestamp - state.prev_timestamp[track_id]
    else
      state.prev_diff
    end
    state = %{state | prev_diff: new_diff}
    state = put_in(state, [:prev_timestamp, track_id], cur_timestamp)

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

  defp update_timestamp(track_id, state, buffer) do
    # Output buffer pts (universal_timestamp) is rewritten to ensure non-decreasing pts value
    # Pts difference between following buffers is the same as in input buffers if they come from the same track
    # Else (when track changes) last rembered difference (prev_diff) is used
    # When prev_diff is not available, switch waits for next keyframe from awaiting track
    # in order to find any usable prev_diff

    cur_timestamp = get_timestamp(buffer, state.type)

    state = put_in(state, [:prev_timestamp, track_id], cur_timestamp)

    if is_nil(state.universal_timestamp) do
      {0, %{state | universal_timestamp: cur_timestamp}}
    else
      new_diff = if Map.has_key?(state.prev_timestamp, track_id) do
        cur_timestamp - state.prev_timestamp[track_id]
      else
        state.prev_diff
      end

      {new_diff, Map.put(state, :prev_diff, new_diff)}
    end
  end

  defp get_timestamp(buffer, _type) when not is_nil(buffer.pts), do: buffer.pts
  defp get_timestamp(buffer, type) when not is_nil(buffer.dts) and type == :audio, do: buffer.dts
  defp get_timestamp(_buffer, _type), do: raise "Buffer does not contain valid pts or dts"
end
