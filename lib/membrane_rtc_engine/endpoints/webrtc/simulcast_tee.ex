defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee do
  @moduledoc false
  use Membrane.Filter

  require Membrane.Logger
  require Membrane.TelemetryMetrics

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched
  }

  alias Membrane.RTC.Engine.Exception.{RequestTrackVariantError, TrackVariantStateError}

  alias Membrane.RTC.Utils

  @supported_codecs [:H264, :VP8]

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this tee is going to forward to other endpoints"
              ]

  def_input_pad :input,
    availability: :on_request,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP,
    options: [
      telemetry_label: [
        spec: Membrane.TelemetryMetrics.label(),
        default: [],
        description: "Label passed to Membrane.TelemetryMetrics functions"
      ]
    ]

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    caps: Membrane.RTP,
    options: [
      default_simulcast_encoding: [
        spec: String.t() | nil,
        default: nil,
        description: """
        Initial encoding that should be sent via this pad.
        `nil` means that the best possible encoding should be used.
        """
      ]
    ]

  @typedoc """
  Notifies that encoding for endpoint with id `endpoint_id` was switched to encoding `encoding`.
  """
  @type encoding_switched_notification_t() ::
          {:encoding_switched, endpoint_id :: any(), encoding :: String.t()}

  @impl true
  def handle_init(opts) do
    if opts.track.encoding not in @supported_codecs do
      raise("""
      #{inspect(__MODULE__)} does not support codec #{inspect(opts.track.encoding)}.
      Supported codecs: #{inspect(@supported_codecs)}
      """)
    end

    {:ok,
     %{
       track: opts.track,
       routes: %{},
       inactive_encodings: opts.track.simulcast_encodings
     }}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:input, {_track_id, encoding}) = pad,
        %{playback_state: playback_state} = ctx,
        state
      ) do
    Utils.telemetry_register(ctx.pads[pad].options.telemetry_label)

    state = update_in(state, [:inactive_encodings], &List.delete(&1, encoding))

    actions =
      if playback_state == :playing do
        for {Pad.ref(:output, _id) = pad, _pad_data} <- ctx.pads, into: [] do
          event = %TrackVariantResumed{variant: encoding}
          {:event, {pad, event}}
        end
      else
        []
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, {:endpoint, _endpoint_id}) = pad,
        %{playback_state: playback_state},
        state
      ) do
    state =
      put_in(state, [:routes, pad], %{
        target_variant: nil,
        current_variant: nil
      })

    actions =
      if playback_state == :playing do
        actions =
          (state.track.simulcast_encodings -- state.inactive_encodings)
          |> Enum.flat_map(&[event: {pad, %TrackVariantResumed{variant: &1}}])

        [caps: {pad, %Membrane.RTP{}}] ++ actions
      else
        []
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(pad, _context, _state) do
    raise("Pad #{inspect(pad)} not allowed for #{inspect(__MODULE__)}")
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {:endpoint, _endpoint_id}) = pad, _context, state) do
    {_route, state} = pop_in(state, [:routes, pad])
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, _rid}), _context, state) do
    # TODO should we do something here?
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(context, state) do
    track_events =
      (state.track.simulcast_encodings -- state.inactive_encodings)
      |> Enum.map(&%TrackVariantResumed{variant: &1})

    actions =
      Enum.flat_map(context.pads, fn
        {Pad.ref(:output, _ref) = pad, _pad_data} ->
          actions = Enum.flat_map(track_events, fn event -> [event: {pad, event}] end)
          [caps: {pad, %Membrane.RTP{}}] ++ actions

        _other ->
          []
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:input, {_track_id, encoding}),
        %TrackVariantPaused{} = event,
        ctx,
        state
      ) do
    state = update_in(state, [:inactive_encodings], &[encoding | &1])

    # reset all target encodings set to `encoding`
    # when `encoding` becomes active again
    # endpoints are expected to request it once again
    state =
      Enum.reduce(state.routes, state, fn
        {output_pad, %{target_encoding: ^encoding}}, state ->
          update_in(state, [:routes, output_pad], &Map.put(&1, :target_encoding, nil))

        _route, state ->
          state
      end)

    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:input, {_track_id, encoding}),
        %TrackVariantResumed{} = event,
        ctx,
        state
      ) do
    state = update_in(state, [:inactive_encodings], &List.delete(&1, encoding))
    {{:ok, forward: event}, state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {:endpoint, endpoint_id}) = output_pad,
        %RequestTrackVariant{variant: requested_variant} = event,
        _ctx,
        state
      ) do
    cond do
      requested_variant in state.inactive_encodings ->
        Membrane.Logger.debug("""
        Endpoint #{endpoint_id} requested track variant: #{requested_variant} but it is inactive. \
        Ignoring.\
        """)

        {:ok, state}

      requested_variant not in state.track.simulcast_encodings ->
        raise RequestTrackVariantError,
          requester: endpoint_id,
          requested_variant: requested_variant,
          track: state.track

      true ->
        Membrane.Logger.info("""
        Endpoint #{endpoint_id} requested track variant #{requested_variant}. \
        Requesting keyframe.
        """)

        handle_track_variant_request(output_pad, event, state)
    end
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_process(Pad.ref(:input, {_track_id, encoding}) = pad, buffer, ctx, state) do
    Utils.emit_packet_arrival_event(
      buffer.payload,
      state.track.encoding,
      ctx.pads[pad].options.telemetry_label
    )

    if encoding in state.inactive_encodings do
      raise TrackVariantStateError, track: state.track, variant: encoding
    end

    is_keyframe = buffer.metadata.is_keyframe

    {actions, state} =
      Enum.flat_map_reduce(state.routes, state, fn
        {output_pad, %{current_variant: ^encoding}}, state ->
          started = ctx.pads[output_pad].start_of_stream?

          actions =
            case started do
              true ->
                [buffer: {output_pad, buffer}]

              false when is_keyframe == true ->
                event = %TrackVariantSwitched{new_variant: encoding}
                [event: {output_pad, event}, buffer: {output_pad, buffer}]

              false ->
                []
            end

          {actions, state}

        {output_pad, %{target_variant: ^encoding}}, state ->
          if is_keyframe do
            state =
              state
              |> put_in([:routes, output_pad, :current_variant], encoding)
              |> put_in([:routes, output_pad, :target_variant], nil)

            event = %TrackVariantSwitched{new_variant: encoding}
            actions = [event: {output_pad, event}, buffer: {output_pad, buffer}]
            {actions, state}
          else
            {[], state}
          end

        {_output_pad, _route}, state ->
          {[], state}
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_end_of_stream(_pad, context, state) do
    all_end_of_streams? =
      context.pads
      |> Enum.filter(fn {_pad_name, pad_data} ->
        pad_data.direction == :input
      end)
      |> Enum.all?(fn {_pad_name, pad_data} ->
        pad_data.end_of_stream?
      end)

    if all_end_of_streams? do
      {{:ok, forward: :end_of_stream}, state}
    else
      {:ok, state}
    end
  end

  defp handle_track_variant_request(output_pad, event, state) do
    %RequestTrackVariant{variant: requested_variant} = event
    pad = Pad.ref(:input, {state.track.id, requested_variant})
    actions = [event: {pad, %Membrane.KeyframeRequestEvent{}}]
    state = put_in(state, [:routes, output_pad, :target_variant], requested_variant)
    {{:ok, actions}, state}
  end
end
