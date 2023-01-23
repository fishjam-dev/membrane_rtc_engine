defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender do
  @moduledoc false

  # TrackSender:
  # * adds `is_keyframe` flag to each buffer's metadata
  # (will be removed after releasing new RTP plugin)
  # * tracks encoding activity

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.{Buffer, Time}
  alias Membrane.RTC.Engine.BitrateEstimator
  alias Membrane.RTC.Engine.Endpoint.WebRTC.VariantTracker

  alias Membrane.RTC.Engine.Event.{
    TrackVariantBitrate,
    TrackVariantPaused,
    TrackVariantResumed,
    VoiceActivityChanged
  }

  alias Membrane.RTC.Engine.Track

  @keyframe_request_interval_ms 500
  @variant_statuses_check_interval_s 1

  @start_variant_check_timer {:start_timer,
                              {:check_variant_statuses,
                               Time.seconds(@variant_statuses_check_interval_s)}}
  @stop_variant_check_timer {:stop_timer, :check_variant_statuses}

  @start_bitrate_estimation_timer {:start_timer, {:estimate_bitrate, Time.seconds(1)}}

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this sender will maintain"
              ],
              variant_bitrates: [
                spec: %{optional(Track.variant()) => non_neg_integer()},
                description: "Bitrate of each variant of track maintained by this sender"
              ],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: [],
                description: "Label passed to Membrane.TelemetryMetrics functions"
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
  def handle_init(%__MODULE__{
        track: track,
        variant_bitrates: variant_bitrates,
        telemetry_label: telemetry_label
      }) do
    {:ok,
     %{
       track: track,
       trackers: %{},
       bitrate_estimators: %{},
       requested_keyframes: MapSet.new(),
       variant_bitrates: variant_bitrates,
       telemetry_label: telemetry_label
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, id), %{playback_state: playback_state}, state) do
    {_track_id, variant} = id
    telemetry_label = state.telemetry_label ++ [track_id: "#{state.track.id}:#{variant}"]
    Membrane.RTC.Utils.telemetry_register(telemetry_label)

    {actions, state} =
      if playback_state == :playing do
        # we need to reset timer and all existing variant
        # trackers to ensure that new tracker's state won't
        # be checked too fast

        state =
          Enum.reduce(state.trackers, state, fn {variant, tracker}, state ->
            put_in(state, [:trackers, variant], VariantTracker.reset(tracker))
          end)

        actions = [@stop_variant_check_timer, @start_variant_check_timer]

        {actions, state}
      else
        {[], state}
      end

    state =
      state
      |> put_in([:bitrate_estimators, variant], BitrateEstimator.new())
      |> put_in([:trackers, variant], VariantTracker.new(variant))

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _id) = pad, %{playback_state: playback_state}, state) do
    actions =
      if playback_state == :playing do
        activate_pad_actions(pad, state)
      else
        []
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {_track_id, variant}), _ctx, state) do
    {_tracker, state} = pop_in(state, [:trackers, variant])
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, variant}), _ctx, state) do
    {_estimator, state} = pop_in(state, [:bitrate_estimators, variant])
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
      |> Enum.flat_map(fn {pad, _pad_data} -> activate_pad_actions(pad, state) end)

    actions = actions ++ [@start_variant_check_timer, @start_bitrate_estimation_timer]

    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:estimate_bitrate, _ctx, state) do
    {estimations, state} =
      state.bitrate_estimators
      |> Enum.reduce({%{}, state}, fn {variant, estimator}, {estimations, state} ->
        case BitrateEstimator.estimate(estimator) do
          {:ok, estimation, estimator} ->
            {Map.put(estimations, variant, estimation),
             put_in(state, [:bitrate_estimators, variant], estimator)}

          {:error, :not_enough_data} ->
            {estimations, state}
        end
      end)

    {{:ok, notify: {:estimation, estimations}}, state}
  end

  @impl true
  def handle_tick(:check_variant_statuses, _ctx, state) do
    {actions, state} =
      Enum.flat_map_reduce(state.trackers, state, fn {variant, tracker}, state ->
        check_variant_status(variant, tracker, state)
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick({:request_keyframe, variant}, _ctx, state) do
    Membrane.Logger.debug("""
    Didn't receive keyframe for variant: #{variant} in #{@keyframe_request_interval_ms}. Retrying.
    """)

    pad = Pad.ref(:input, {state.track.id, variant})
    actions = [event: {pad, %Membrane.KeyframeRequestEvent{}}]

    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, _id),
        %Membrane.KeyframeRequestEvent{},
        _ctx,
        %{track: %Track{type: :audio}} = state
      ) do
    {:ok, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {track_id, variant}),
        %Membrane.KeyframeRequestEvent{} = event,
        _ctx,
        state
      ) do
    {actions, state} =
      cond do
        MapSet.member?(state.requested_keyframes, variant) ->
          Membrane.Logger.debug("Requested keyframe but we are already awaiting it. Ignoring.")
          {[], state}

        state.trackers[variant].status == :active ->
          Membrane.Logger.debug("Requesting keyframe for #{inspect(variant)}")
          requested_keyframes = MapSet.put(state.requested_keyframes, variant)
          state = %{state | requested_keyframes: requested_keyframes}

          interval = Time.milliseconds(@keyframe_request_interval_ms)

          actions = [
            event: {Pad.ref(:input, {track_id, variant}), event},
            start_timer: {{:request_keyframe, variant}, interval}
          ]

          {actions, state}

        true ->
          {[], state}
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:input, {track_id, variant}),
        %Membrane.RTP.VadEvent{vad: vad},
        _ctx,
        state
      ) do
    output_pad = Pad.ref(:output, {track_id, variant})

    event = %VoiceActivityChanged{
      voice_activity: vad
    }

    {{:ok, event: {output_pad, event}}, state}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_process(
        Pad.ref(:input, {_track_id, variant}) = input_pad,
        buffer,
        ctx,
        %{track: track} = state
      ) do
    Membrane.RTC.Utils.emit_packet_arrival_event(
      buffer.payload,
      state.track.encoding,
      state.telemetry_label
    )

    state =
      state
      |> update_in([:bitrate_estimators, variant], &BitrateEstimator.process(&1, buffer))
      |> update_in([:trackers, variant], &VariantTracker.increment_samples(&1))

    buffer = add_is_keyframe_flag(buffer, track)

    {actions, state} =
      if MapSet.member?(state.requested_keyframes, variant) and buffer.metadata.is_keyframe do
        Membrane.Logger.debug(
          "Received keyframe for #{variant}. Removing it from keyframe request queue."
        )

        requested_keyframes = MapSet.delete(state.requested_keyframes, variant)
        actions = [stop_timer: {:request_keyframe, variant}]
        {actions, %{state | requested_keyframes: requested_keyframes}}
      else
        {[], state}
      end

    output_pad = to_output_pad(input_pad)
    # FIXME
    # it's possible that we will have
    # input pad but we won't have
    # corresponding output pad yet
    # (refer to MC-68)
    actions =
      if Map.has_key?(ctx.pads, output_pad) and state.trackers[variant].status == :active do
        actions ++ [buffer: {output_pad, buffer}]
      else
        actions
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_end_of_stream(input_pad, ctx, state) do
    output_pad = to_output_pad(input_pad)

    if Map.has_key?(ctx.pads, output_pad) do
      {{:ok, end_of_stream: output_pad}, state}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_other({:variant_bitrates, variant_bitrates}, ctx, state) do
    state = %{state | variant_bitrates: Map.merge(state.variant_bitrates, variant_bitrates)}

    actions =
      ctx.pads
      |> Enum.flat_map(fn
        {{Membrane.Pad, :output, {_track_id, variant}} = pad, _pad_data} ->
          case Map.get(variant_bitrates, variant) do
            nil -> []
            bitrate -> [event: {pad, %TrackVariantBitrate{variant: variant, bitrate: bitrate}}]
          end

        _other ->
          []
      end)

    {{:ok, actions}, state}
  end

  defp check_variant_status(variant, tracker, state) do
    pad = Pad.ref(:output, {state.track.id, variant})

    {actions, tracker, state} =
      case VariantTracker.check_variant_status(tracker) do
        {:ok, tracker} ->
          {[], tracker, state}

        {:status_changed, tracker, :active} ->
          event = %TrackVariantResumed{variant: variant}
          {[event: {pad, event}], tracker, state}

        {:status_changed, tracker, :inactive} ->
          event = %TrackVariantPaused{variant: variant}

          actions =
            if MapSet.member?(state.requested_keyframes, variant) do
              [stop_timer: {:request_keyframe, variant}]
            else
              []
            end

          state = Map.update!(state, :requested_keyframes, &MapSet.delete(&1, variant))
          {actions ++ [event: {pad, event}], tracker, state}
      end

    state = put_in(state, [:trackers, variant], tracker)
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

  defp activate_pad_actions(Pad.ref(:output, {_track_id, variant}) = pad, state) do
    [
      caps: {pad, %Membrane.RTP{}},
      event: {pad, %TrackVariantResumed{variant: variant}},
      event:
        {pad,
         %TrackVariantBitrate{
           variant: variant,
           bitrate: Map.get(state.variant_bitrates, variant)
         }}
    ]
  end

  defp to_output_pad(Pad.ref(:input, {_track_id, _encoding} = pad_id)) do
    Pad.ref(:output, pad_id)
  end
end
