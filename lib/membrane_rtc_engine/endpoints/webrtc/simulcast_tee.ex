defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastTee do
  @moduledoc false
  use Membrane.Filter

  alias Membrane.Time
  alias Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTracker
  alias Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder
  alias Membrane.RTC.Utils

  require Membrane.Logger
  require Membrane.TelemetryMetrics

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

    trackers = Map.new(opts.track.simulcast_encodings, &{&1, EncodingTracker.new(&1)})

    {:ok,
     %{
       track: opts.track,
       forwarders: %{},
       trackers: trackers,
       inactive_encodings: [],
       layer_bandwidth_estimation_buffer: %{}
     }}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, {_track_id, encoding}) = pad, ctx, state) do
    Utils.telemetry_register(ctx.pads[pad].options.telemetry_label)

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
  def handle_pad_removed(Pad.ref(:input, {_track_id, rid}), _context, state) do
    {_tracker, state} = pop_in(state, [:trackers, rid])
    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(context, state) do
    caps =
      Enum.flat_map(context.pads, fn
        {Pad.ref(:output, _ref) = pad, _pad_data} -> [caps: {pad, %Membrane.RTP{}}]
        _other -> []
      end)

    start_timers = [
      start_timer: {:check_encoding_statuses, Time.seconds(1)},
      start_timer: {:bandwidth_estimation_timer, Time.seconds(1)}
    ]

    {{:ok, start_timers ++ caps}, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, {_track_id, encoding}) = pad, buffer, ctx, state) do
    Utils.emit_packet_arrival_event(
      buffer.payload,
      state.track.encoding,
      ctx.pads[pad].options.telemetry_label
    )

    updated_estimation_buffer =
      Map.update(state.layer_bandwidth_estimation_buffer, pad, [buffer], &[buffer | &1])

    state =
      state
      |> update_in([:trackers, encoding], &EncodingTracker.increment_samples(&1))
      |> Map.put(:layer_bandwidth_estimation_buffer, updated_estimation_buffer)

    {actions, state} =
      Enum.flat_map_reduce(state.forwarders, state, fn
        {endpoint_id, forwarder}, state ->
          {forwarder, actions} = Forwarder.process(forwarder, buffer, encoding, endpoint_id)
          state = put_in(state, [:forwarders, endpoint_id], forwarder)
          {actions, state}
      end)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:bandwidth_estimation_timer, _ctx, state) do
    estimation =
      state.layer_bandwidth_estimation_buffer
      |> Map.new(fn {Pad.ref(:input, {_track_id, layer}), buffers} ->
        bitrate =
          if length(buffers) >= 10 do
            buffers
            |> Enum.map(&byte_size(&1.payload))
            |> Enum.sum()
            |> then(&(&1 * 8 / 1024))
          else
            0
          end

        {layer, bitrate}
      end)

    {{:ok, notify: {:bandwidth_estimation, estimation}},
     %{state | layer_bandwidth_estimation_buffer: %{}}}
  end

  @impl true
  def handle_tick(:check_encoding_statuses, ctx, state) do
    {actions, new_state} =
      state.trackers
      |> Enum.reduce(state, fn {rid, tracker}, state ->
        check_encoding_status(rid, tracker, state)
      end)
      |> generate_keyframe_requests(state, ctx)

    {{:ok, actions}, new_state}
  end

  defp check_encoding_status(rid, tracker, state) do
    case EncodingTracker.check_encoding_status(tracker) do
      {:ok, tracker} ->
        put_in(state, [:trackers, rid], tracker)

      {:status_changed, tracker, :active} ->
        state =
          state.forwarders
          |> Enum.reduce(state, fn {endpoint_id, forwarder}, state ->
            put_in(state, [:forwarders, endpoint_id], Forwarder.encoding_active(forwarder, rid))
          end)

        state
        |> update_in([:inactive_encodings], &List.delete(&1, rid))
        |> put_in([:trackers, rid], tracker)

      {:status_changed, tracker, :inactive} ->
        state =
          state.forwarders
          |> Enum.reduce(state, fn {endpoint_id, forwarder}, state ->
            forwarder = Forwarder.encoding_inactive(forwarder, rid)
            put_in(state, [:forwarders, endpoint_id], forwarder)
          end)

        state
        |> update_in([:inactive_encodings], &[rid | &1])
        |> put_in([:trackers, rid], tracker)
    end
  end

  @impl true
  def handle_other({:bandwidth_limitation, limitations}, _ctx, state) do
    forwarders =
      Map.new(state.forwarders, fn {endpoint_id, forwarder} ->
        {endpoint_id, %{forwarder | allowed_encodings: Map.get(limitations, endpoint_id)}}
      end)

    {:ok, %{state | forwarders: forwarders}}
  end

  @impl true
  def handle_other({:select_encoding, {endpoint_id, encoding}}, ctx, state) do
    forwarder =
      state.forwarders[endpoint_id]
      |> Forwarder.select_encoding(encoding)

    actions =
      if forwarder.queued_encoding == encoding do
        ctx.pads
        |> Map.keys()
        |> Enum.find(fn
          Pad.ref(:input, {_track_id, ^encoding}) -> true
          _other -> false
        end)
        |> case do
          nil ->
            Membrane.Logger.warn("Cannot find pad for requested encoding #{encoding}")
            []

          pad_ref ->
            Membrane.Logger.warn("KeyframeRequest for encoding #{encoding}")
            [event: {pad_ref, %Membrane.KeyframeRequestEvent{}}]
        end
      else
        []
      end

    state = put_in(state, [:forwarders, endpoint_id], forwarder)
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

  defp generate_keyframe_requests(state, old_state, ctx) do
    find_pad = fn {_key, %{queued_encoding: encoding}} ->
      ctx.pads
      |> Map.keys()
      |> Enum.find(&match?(Pad.ref(:input, {_track_id, ^encoding}), &1))
    end

    state.forwarders
    |> Enum.filter(fn {key, forwarder} ->
      # Queued encoding has been changed
      # And this forwarder requires a keyframe request
      forwarder.queued_encoding != old_state.forwarders[key].queued_encoding and
        forwarder.selected_encoding != forwarder.queued_encoding and
        not is_nil(forwarder.queued_encoding)
    end)
    |> Enum.uniq()
    |> Enum.map(find_pad)
    |> Enum.reject(&is_nil/1)
    |> Enum.flat_map(&[event: {&1, %Membrane.KeyframeRequestEvent{}}])
    |> then(&{&1, state})
  end
end
