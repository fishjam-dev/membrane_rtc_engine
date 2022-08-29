defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender do
  @moduledoc false

  # TrackSender:
  # * adds `is_keyframe` flag to each buffer's metadata
  # (will be removed after releasing new RTP plugin)
  # * tracks encoding activity

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.{Buffer, Time}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTracker
  alias Membrane.RTC.Engine.Event.{TrackVariantPaused, TrackVariantResumed}
  alias Membrane.RTC.Engine.Track

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this sender will maintain"
              ]

  def_input_pad :input,
    availability: :on_request,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  def_output_pad :output,
    availability: :on_request,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  @impl true
  def handle_init(%__MODULE__{track: track}) do
    {:ok, %{track: track, trackers: %{}}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, encoding}), _ctx, state) do
    state = put_in(state, [:trackers, encoding], EncodingTracker.new(encoding))
    {:ok, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, {_track_id, _encoding}) = pad,
        %{playback_state: playback_state},
        state
      ) do
    actions =
      if playback_state == :playing do
        [caps: {pad, %Membrane.RTP{}}]
      else
        []
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {_track_id, encoding}), _ctx, state) do
    {_tracker, state} = pop_in(state, [:trackers, encoding])
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(_pad, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_caps(_pad, _caps, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(ctx, state) do
    actions =
      Enum.flat_map(ctx.pads, fn
        {Pad.ref(:output, _ref) = pad, _pad_data} -> [caps: {pad, %Membrane.RTP{}}]
        _other -> []
      end)

    # start timer only for simulcast tracks
    actions =
      actions ++
        if state.track.simulcast_encodings != [] do
          [start_timer: {:check_encoding_statuses, Time.seconds(1)}]
        else
          []
        end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:check_encoding_statuses, _ctx, state) do
    {actions, state} =
      Enum.flat_map_reduce(state.trackers, state, fn {encoding, tracker}, state ->
        check_encoding_status(encoding, tracker, state)
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_process(
        Pad.ref(:input, {_track_id, encoding}) = input_pad,
        buffer,
        ctx,
        %{track: track} = state
      ) do
    # update encoding tracker only for simulcast tracks
    state =
      if encoding == nil do
        state
      else
        update_in(state, [:trackers, encoding], &EncodingTracker.increment_samples(&1))
      end

    buffer = add_is_keyframe_flag(buffer, track)
    output_pad = to_output_pad(input_pad)

    # FIXME
    # it's possible that we will have
    # input pad but we won't have
    # corresponding output pad yet
    # (refer to MC-68)
    if Map.has_key?(ctx.pads, output_pad) do
      {{:ok, buffer: {output_pad, buffer}}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_end_of_stream(input_pad, _ctx, state) do
    output_pad = to_output_pad(input_pad)
    {{:ok, end_of_stream: output_pad}, state}
  end

  defp check_encoding_status(encoding, tracker, state) do
    pad = Pad.ref(:output, {state.track.id, encoding})

    {actions, tracker} =
      case EncodingTracker.check_encoding_status(tracker) do
        {:ok, tracker} ->
          {[], tracker}

        {:status_changed, tracker, :active} ->
          event = %TrackVariantResumed{}
          {[event: {pad, event}], tracker}

        {:status_changed, tracker, :inactive} ->
          event = %TrackVariantPaused{}
          {[event: {pad, event}], tracker}
      end

    state = put_in(state, [:trackers, encoding], tracker)
    {actions, state}
  end

  defp add_is_keyframe_flag(buffer, %Track{encoding: encoding}) do
    is_keyframe =
      case encoding do
        :OPUS -> true
        :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buffer.payload)
        :VP8 -> Membrane.RTP.VP8.Utils.is_keyframe(buffer.payload)
      end

    new_metadata = Map.put(buffer.metadata, :is_keyframe, is_keyframe)
    %Buffer{buffer | metadata: new_metadata}
  end

  defp to_output_pad(Pad.ref(:input, {_track_id, _encoding} = pad_id)) do
    Pad.ref(:output, pad_id)
  end
end
