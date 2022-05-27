defmodule Membrane.RTC.Engine.FilterTee do
  @moduledoc false

  # Element for forwarding buffers to multiple output pads. If no output
  # pad is linked, buffers are dropped.

  # It has got built-in mechanism for limiting forwarding video buffers.
  # It reads from ETS table on which pads it should forward buffers.

  # Counter is used for passing a single packets once in a while.
  # It is necessary for SRTP as they can update their ROCs
  # based on sequence numbers and when we drop to many packets we may roll it over.

  use Membrane.Filter

  def_options track_id: [
                spec: String.t(),
                description: "Id of track for which tee was created"
              ],
              ets_name: [
                spec: String.t(),
                description:
                  "Name of ETS table from which Tee will read to which pads it should send buffers",
                default: "table"
              ],
              type: [
                spec: :audio | :video,
                description: "Type of track which buffers tee is forwarding"
              ],
              codec: [
                type: :atom,
                spec: [:H264 | :VP8 | :OPUS],
                description: "Codec of track #{inspect(__MODULE__)} will forward."
              ],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: [],
                description: "Label passed to Membrane.TelemetryMetrics functions"
              ]

  def_input_pad :input,
    availability: :always,
    mode: :pull,
    demand_mode: :auto,
    caps: :any

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    caps: :any

  @impl true
  def handle_init(opts) do
    Membrane.RTC.Utils.telemetry_register(
      opts.telemetry_label,
      opts.codec
    )

    {:ok,
     %{
       ets_name: :"#{opts.ets_name}",
       track_id: opts.track_id,
       counter: 0,
       type: opts.type,
       forward_to: MapSet.new(),
       codec: opts.codec,
       telemetry_label: opts.telemetry_label
     }}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, _ctx, %{type: :audio} = state) do
    Membrane.RTC.Utils.emit_telemetry_event_with_packet_mesaurments(
      buffer.payload,
      state.telemetry_label,
      state.codec
    )

    {{:ok, forward: buffer}, state}
  end

  @impl true
  def handle_process(
        :input,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{type: :video, counter: 1000} = state
      ) do
    Membrane.RTC.Utils.emit_telemetry_event_with_packet_mesaurments(
      buffer.payload,
      state.telemetry_label,
      state.codec
    )

    {{:ok, forward: buffer}, %{state | counter: 0}}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, ctx, %{type: :video} = state) do
    Membrane.RTC.Utils.emit_telemetry_event_with_packet_mesaurments(
      buffer.payload,
      state.telemetry_label,
      state.codec
    )

    pads =
      ctx.pads
      |> Map.keys()
      |> Enum.filter(fn
        {Membrane.Pad, :output, {:endpoint, _endpoint_id} = endpoint_name} ->
          MapSet.member?(state.forward_to, endpoint_name)

        {Membrane.Pad, :output, _ref} ->
          true

        _other ->
          false
      end)

    actions = Enum.map(pads, &{:buffer, {&1, buffer}})
    {{:ok, actions}, %{state | counter: state.counter + 1}}
  end

  @impl true
  def handle_other(:track_priorities_updated, _ctx, state) do
    forward_to =
      case :ets.lookup(state.ets_name, state.track_id) do
        [{_track_id, endpoint_names} | _] ->
          MapSet.new(endpoint_names)

        [] ->
          MapSet.new()
      end

    {:ok, %{state | forward_to: forward_to}}
  end
end
