defmodule Membrane.RTC.Engine.PushOutputTee do
  @moduledoc """
  Element forwarding packets to multiple push outputs.
  """
  use Membrane.Filter

  def_options codec: [
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
    Membrane.RTC.Utils.telemetry_register(opts.telemetry_label)

    {:ok,
     %{
       codec: opts.codec,
       caps: nil,
       telemetry_label: opts.telemetry_label
     }}
  end

  @impl true
  def handle_caps(_pad, caps, _ctx, state) do
    {{:ok, forward: caps}, %{state | caps: caps}}
  end

  @impl true
  def handle_end_of_stream(_pad, ctx, state) do
    depayloading_filter_exists =
      ctx.pads
      |> Map.keys()
      |> Enum.find(fn
        {Membrane.Pad, :output, _ref} -> true
        _other -> false
      end) != nil

    event =
      if depayloading_filter_exists do
        [event: {:input, %Membrane.RTC.Engine.Event.EndProcessing{}}]
      else
        []
      end

    IO.inspect(event, label: :end_of_stream)

    {{:ok, [{:forward, :end_of_stream}] ++ event}, state}
  end

  @impl true
  def handle_event(Pad.ref(:input, _ref), event, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_event(Pad.ref(:output, _ref), event, _ctx, state) do
    IO.inspect(event, label: :handle_event)
    {{:ok, event: {:input, event}}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _ref), _ctx, %{caps: nil} = state) do
    {:ok, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _ref) = pad, _ctx, %{caps: caps} = state) do
    {{:ok, caps: {pad, caps}}, state}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, _ctx, state) do
    Membrane.RTC.Utils.emit_packet_arrival_event(
      buffer.payload,
      state.codec,
      state.telemetry_label
    )

    {{:ok, forward: buffer}, state}
  end
end
