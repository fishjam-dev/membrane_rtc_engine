defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee do
  @moduledoc false
  use Membrane.Filter

  require Membrane.Logger
  require Membrane.TelemetryMetrics

  alias Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder
  alias Membrane.RTC.Engine.Event.{TrackVariantPaused, TrackVariantResumed, TrackVariantSwitched}

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
       forwarders: %{},
       inactive_encodings: MapSet.new(opts.track.simulcast_encodings)
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, encoding}) = pad, ctx, state) do
    Utils.telemetry_register(ctx.pads[pad].options.telemetry_label)

    state = update_in(state, [:inactive_encodings], &MapSet.delete(&1, encoding))

    state =
      Enum.reduce(state.forwarders, state, fn {endpoint_id, forwarder}, state ->
        forwarder = Forwarder.encoding_active(forwarder, encoding)
        put_in(state, [:forwarders, endpoint_id], forwarder)
      end)

    actions =
      state.forwarders
      |> Map.values()
      # See if there are any forwarders that await switching to the layer delivered on the pad
      |> Enum.find(&(&1.queued_encoding == encoding))
      |> case do
        nil -> []
        _otherwise -> [event: {pad, %Membrane.KeyframeRequestEvent{}}]
      end

    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, {:endpoint, endpoint_id}) = pad,
        %{playback_state: playback_state} = context,
        state
      ) do
    forwarder =
      Forwarder.new(
        state.track.encoding,
        state.track.clock_rate,
        state.track.simulcast_encodings,
        context.options[:default_simulcast_encoding]
      )

    forwarder =
      Enum.reduce(state.inactive_encodings, forwarder, fn encoding, forwarder ->
        Forwarder.encoding_inactive(forwarder, encoding)
      end)

    state = put_in(state, [:forwarders, endpoint_id], forwarder)

    actions =
      if playback_state == :playing do
        [caps: {pad, %Membrane.RTP{}}]
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
  def handle_pad_removed(Pad.ref(:output, {:endpoint, endpoint_id}), _context, state) do
    {_forwarder, state} = pop_in(state, [:forwarders, endpoint_id])
    {:ok, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, {_track_id, _rid}), _context, state) do
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(context, state) do
    caps =
      Enum.flat_map(context.pads, fn
        {Pad.ref(:output, _ref) = pad, _pad_data} -> [caps: {pad, %Membrane.RTP{}}]
        _other -> []
      end)

    {{:ok, caps}, state}
  end

  @impl true
  def handle_event(Pad.ref(:input, {_track_id, encoding}), event, ctx, state) do
    {actions, state} = handle_input_event(event, encoding, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  defp handle_input_event(%TrackVariantPaused{}, encoding, ctx, state) do
    new_state = update_in(state, [:inactive_encodings], &MapSet.put(&1, encoding))

    new_state =
      Enum.reduce(state.forwarders, new_state, fn {endpoint_id, forwarder}, new_state ->
        put_in(
          new_state,
          [:forwarders, endpoint_id],
          Forwarder.encoding_inactive(forwarder, encoding)
        )
      end)

    actions = generate_keyframe_requests(new_state, state, ctx)

    {actions, new_state}
  end

  defp handle_input_event(%TrackVariantResumed{}, encoding, ctx, state) do
    new_state = update_in(state, [:inactive_encodings], &MapSet.delete(&1, encoding))

    new_state =
      Enum.reduce(state.forwarders, new_state, fn {endpoint_id, forwarder}, new_state ->
        put_in(
          new_state,
          [:forwarders, endpoint_id],
          Forwarder.encoding_active(forwarder, encoding)
        )
      end)

    actions = generate_keyframe_requests(new_state, state, ctx)

    {actions, new_state}
  end

  @impl true
  def handle_process(Pad.ref(:input, {_track_id, encoding}) = pad, buffer, ctx, state) do
    Utils.emit_packet_arrival_event(
      buffer.payload,
      state.track.encoding,
      ctx.pads[pad].options.telemetry_label
    )

    {actions, state} =
      Enum.flat_map_reduce(state.forwarders, state, fn
        {endpoint_id, forwarder}, state ->
          {forwarder, actions} = Forwarder.process(forwarder, buffer, encoding, endpoint_id)

          # FIXME this is a temporar solution, it will be removed
          # in subsequent PRs
          #
          # if that is a first buffer we are forwarding
          # add TrackVariantSwitched event to actions
          output_pad = Pad.ref(:output, {:endpoint, endpoint_id})
          started = ctx.pads[output_pad].start_of_stream?

          {actions, state} =
            case actions do
              [{:buffer, {^output_pad, _buffer}}] when started == false ->
                event = %TrackVariantSwitched{new_variant: encoding}
                actions = [event: {output_pad, event}] ++ actions
                {actions, state}

              other ->
                {other, state}
            end

          state = put_in(state, [:forwarders, endpoint_id], forwarder)
          {actions, state}
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_other({:select_encoding, {endpoint_id, encoding}}, ctx, state) do
    Membrane.Logger.debug("Selecting encoding #{encoding} for endpoint #{endpoint_id}")
    forwarder = Forwarder.select_encoding(state.forwarders[endpoint_id], encoding)
    new_state = put_in(state, [:forwarders, endpoint_id], forwarder)

    actions = generate_keyframe_requests(new_state, state, ctx)
    {{:ok, actions}, new_state}
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

  defp generate_keyframe_requests(state, old_state, ctx) do
    state.forwarders
    |> Enum.filter(fn {endpoint_id, forwarder} ->
      old_forwarder = old_state.forwarders[endpoint_id]
      old_encoding = Forwarder.get_status(old_forwarder).awaiting_keyframe
      new_encoding = Forwarder.get_status(forwarder).awaiting_keyframe

      not is_nil(new_encoding) and old_encoding != new_encoding
    end)
    |> Enum.uniq()
    |> Enum.map(fn forwarder -> find_pad(forwarder, ctx) end)
    |> Enum.reject(&is_nil/1)
    |> tap(fn events ->
      Enum.each(events, &Membrane.Logger.debug("Sending keyframe request on pad #{inspect(&1)}"))
    end)
    |> Enum.map(&{:event, {&1, %Membrane.KeyframeRequestEvent{}}})
  end

  defp find_pad({_key, forwarder}, ctx) do
    %Forwarder.Status{awaiting_keyframe: encoding} = Forwarder.get_status(forwarder)

    ctx.pads
    |> Map.keys()
    |> Enum.find(&match?(Pad.ref(:input, {_track_id, ^encoding}), &1))
  end
end
