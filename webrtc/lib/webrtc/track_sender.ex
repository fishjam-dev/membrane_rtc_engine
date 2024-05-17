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
  alias Membrane.RTC.Engine.Endpoint.WebRTC.{Metrics, VariantTracker}

  alias Membrane.RTC.Engine.Event.{
    TrackVariantBitrate,
    TrackVariantPaused,
    TrackVariantResumed,
    VoiceActivityChanged
  }

  alias Membrane.RTC.Engine.Track
  alias Membrane.RTCP.SenderReportPacket
  alias Membrane.RTCPEvent

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
              ],
              is_keyframe_fun: [
                spec: (Membrane.Buffer.t(), Track.encoding() -> boolean()),
                default: &Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender.keyframe?/2,
                description:
                  "Function checking whether a given buffer contains a keyframe in its payload."
              ]

  def_input_pad :input,
    availability: :on_request,
    accepted_format: Membrane.RTP

  def_output_pad :output,
    availability: :on_request,
    accepted_format: Membrane.RTP

  @impl true
  def handle_init(_ctx, %__MODULE__{
        track: track,
        variant_bitrates: variant_bitrates,
        telemetry_label: telemetry_label,
        is_keyframe_fun: is_keyframe_fun
      }) do
    {[],
     %{
       track: track,
       trackers: %{},
       bitrate_estimators: %{},
       requested_keyframes: MapSet.new(),
       variant_bitrates: variant_bitrates,
       telemetry_label: telemetry_label,
       is_keyframe_fun: is_keyframe_fun
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, id), %{playback: playback}, state) do
    {_track_id, variant} = id
    telemetry_label = state.telemetry_label ++ [track_id: "#{state.track.id}:#{variant}"]
    Metrics.telemetry_register(telemetry_label)

    {actions, state} =
      if playback == :playing and Track.simulcast?(state.track) do
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

    tracker =
      if Track.simulcast?(state.track) do
        VariantTracker.new(variant)
      else
        # assume that non-simulcast tracks are always active
        # we also don't start variant tracker timer
        # and don't increment samples for them
        VariantTracker.new(variant, 0)
      end

    state =
      state
      |> put_in([:bitrate_estimators, variant], BitrateEstimator.new())
      |> put_in([:trackers, variant], tracker)

    {actions, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _id) = pad, %{playback: playback}, state) do
    actions =
      if playback == :playing do
        activate_pad_actions(pad, state)
      else
        []
      end

    {actions, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {_track_id, variant}), _ctx, state) do
    {_tracker, state} = pop_in(state, [:trackers, variant])
    {[], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, variant}), _ctx, state) do
    {_estimator, state} = pop_in(state, [:bitrate_estimators, variant])
    {[], state}
  end

  @impl true
  def handle_stream_format(_pad, _format, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_playing(ctx, state) do
    actions =
      ctx.pads
      |> Enum.filter(fn {_pad_id, %{name: name}} -> name == :output end)
      |> Enum.flat_map(fn {pad, _pad_data} -> activate_pad_actions(pad, state) end)

    actions =
      if Track.simulcast?(state.track) do
        actions ++ [@start_variant_check_timer, @start_bitrate_estimation_timer]
      else
        actions ++ [@start_bitrate_estimation_timer]
      end

    {actions, state}
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

    {[notify_parent: {:estimation, estimations}], state}
  end

  @impl true
  def handle_tick(:check_variant_statuses, _ctx, state) do
    Enum.flat_map_reduce(state.trackers, state, fn {variant, tracker}, state ->
      check_variant_status(variant, tracker, state)
    end)
  end

  @impl true
  def handle_tick({:request_keyframe, variant}, _ctx, state) do
    Membrane.Logger.debug("""
    Didn't receive keyframe for variant: #{variant} in #{@keyframe_request_interval_ms}. Retrying.
    """)

    pad = Pad.ref(:input, {state.track.id, variant})
    actions = [event: {pad, %Membrane.KeyframeRequestEvent{}}]

    {actions, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, _id),
        %Membrane.KeyframeRequestEvent{},
        _ctx,
        %{track: %Track{type: :audio}} = state
      ) do
    {[], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {track_id, variant}),
        %Membrane.KeyframeRequestEvent{} = event,
        _ctx,
        state
      ) do
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

    {[event: {output_pad, event}], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:input, {track_id, variant}),
        %RTCPEvent{rtcp: %SenderReportPacket{}} = event,
        _ctx,
        state
      ) do
    {[event: {Pad.ref(:output, {track_id, variant}), event}], state}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_buffer(
        Pad.ref(:input, {_track_id, variant}) = input_pad,
        buffer,
        ctx,
        %{track: track} = state
      ) do
    Metrics.emit_packet_arrival_event(
      buffer.payload,
      state.track.encoding,
      state.telemetry_label
    )

    state =
      if Track.simulcast?(track) do
        update_in(state, [:trackers, variant], &VariantTracker.increment_samples(&1))
      else
        state
      end
      |> update_in([:bitrate_estimators, variant], &BitrateEstimator.process(&1, buffer))

    new_metadata =
      Map.put(buffer.metadata, :is_keyframe, state.is_keyframe_fun.(buffer, track.encoding))

    buffer = %Buffer{buffer | metadata: new_metadata}

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

    {actions, state}
  end

  @impl true
  def handle_end_of_stream(input_pad, ctx, state) do
    output_pad = to_output_pad(input_pad)

    if Map.has_key?(ctx.pads, output_pad) do
      {[end_of_stream: output_pad], state}
    else
      {[], state}
    end
  end

  @impl true
  def handle_parent_notification({:variant_bitrates, variant_bitrates}, ctx, state) do
    state = %{state | variant_bitrates: Map.merge(state.variant_bitrates, variant_bitrates)}

    actions =
      ctx.pads
      |> Enum.filter(fn
        {Pad.ref(:output, {_track_id, variant}), _pad_data} ->
          Map.has_key?(variant_bitrates, variant)

        _other ->
          false
      end)
      |> Enum.flat_map(fn {pad, _pad_data} -> get_variant_bitrate_action(pad, state) end)

    {actions, state}
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

  @doc """
  Default function checking whether a given buffer contains a keyframe in its payload.
  """
  @spec keyframe?(Membrane.Buffer.t(), Track.encoding()) :: boolean()
  def keyframe?(buffer, encoding) do
    case encoding do
      :OPUS -> true
      :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buffer.payload)
      :VP8 -> Membrane.RTP.VP8.Utils.is_keyframe(buffer.payload)
    end
  end

  defp activate_pad_actions(Pad.ref(:output, {_track_id, variant}) = pad, state) do
    [
      stream_format: {pad, %Membrane.RTP{}},
      event: {pad, %TrackVariantResumed{variant: variant}}
    ] ++ get_variant_bitrate_action(pad, state)
  end

  defp get_variant_bitrate_action(Pad.ref(:output, {_track_id, variant}) = pad, state) do
    case Map.fetch(state.variant_bitrates, variant) do
      {:ok, bitrate} -> [event: {pad, %TrackVariantBitrate{variant: variant, bitrate: bitrate}}]
      :error -> []
    end
  end

  defp to_output_pad(Pad.ref(:input, {_track_id, _encoding} = pad_id)) do
    Pad.ref(:output, pad_id)
  end
end
