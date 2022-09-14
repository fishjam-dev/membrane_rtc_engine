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
    {:ok, %{track: track, trackers: %{}, requested_keyframes: MapSet.new()}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, encoding}), _ctx, state) do
    state = put_in(state, [:trackers, encoding], EncodingTracker.new(encoding))
    {:ok, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, _id) = pad,
        %{playback_state: playback_state},
        state
      ) do
    actions =
      if playback_state == :playing do
        activate_pad_actions(pad)
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
      ctx.pads
      |> Enum.filter(fn {_pad_id, %{name: name}} -> name == :output end)
      |> Enum.flat_map(fn {pad, _pad_data} -> activate_pad_actions(pad) end)

    # start timer only for simulcast tracks
    actions =
      if state.track.simulcast_encodings != [] do
        actions ++ [start_timer: {:check_encoding_statuses, Time.seconds(1)}]
      else
        actions
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
  def handle_event(
        Pad.ref(:output, {track_id, encoding}),
        %Membrane.KeyframeRequestEvent{} = event,
        _ctx,
        state
      ) do
    {actions, state} =
      if MapSet.member?(state.requested_keyframes, encoding) do
        Membrane.Logger.info("Requested keyframe but we are already awaiting it. Ignoring.")
        {[], state}
      else
        Membrane.Logger.debug("Requesting keyframe for #{inspect(encoding)}")
        requested_keyframes = MapSet.put(state.requested_keyframes, encoding)
        state = %{state | requested_keyframes: requested_keyframes}
        actions = [event: {Pad.ref(:input, {track_id, encoding}), event}]
        {actions, state}
      end

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

    state =
      if MapSet.member?(state.requested_keyframes, encoding) and buffer.metadata.is_keyframe do
        Membrane.Logger.debug(
          "Received keyframe for #{encoding}. Removing it from keyframe request queue."
        )

        requested_keyframes = MapSet.delete(state.requested_keyframes, encoding)
        %{state | requested_keyframes: requested_keyframes}
      else
        state
      end

    output_pad = to_output_pad(input_pad)
    # FIXME
    # it's possible that we will have
    # input pad but we won't have
    # corresponding output pad yet
    # (refer to MC-68)
    if Map.has_key?(ctx.pads, output_pad) and state.trackers[encoding].status == :active do
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

    {actions, tracker, state} =
      case EncodingTracker.check_encoding_status(tracker) do
        {:ok, tracker} ->
          {[], tracker, state}

        {:status_changed, tracker, :active} ->
          event = %TrackVariantResumed{variant: encoding}
          {[event: {pad, event}], tracker, state}

        {:status_changed, tracker, :inactive} ->
          event = %TrackVariantPaused{variant: encoding}
          state = Map.update!(state, :requested_keyframes, &MapSet.delete(&1, encoding))
          {[event: {pad, event}], tracker, state}
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

  defp activate_pad_actions(Pad.ref(:output, {_track_id, encoding}) = pad) do
    [caps: {pad, %Membrane.RTP{}}, event: {pad, %TrackVariantResumed{variant: encoding}}]
  end

  defp to_output_pad(Pad.ref(:input, {_track_id, _encoding} = pad_id)) do
    Pad.ref(:output, pad_id)
  end
end
