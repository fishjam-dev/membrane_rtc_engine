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
       telemetry_label: opts.telemetry_label,
       stream_ended?: false
     }}
  end

  @impl true
  def handle_caps(_pad, caps, _ctx, state) do
    {{:ok, forward: caps}, %{state | caps: caps}}
  end

  @impl true
  def handle_end_of_stream(_pad, ctx, state) do
    exist_next_tee? =
      ctx.pads
      |> Map.keys()
      |> Enum.any?(fn
        Pad.ref(:output, {:endpoint, :raw_format_filter}) -> true
        _other -> false
      end)

    unless exist_next_tee? do
      Process.send_after(self(), :end_processing, 5_000)
    end

    {{:ok, forward: :end_of_stream}, %{state | stream_ended?: true}}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, _ref) = pad,
        _ctx,
        %{caps: caps, stream_ended?: stream_ended?} = state
      ) do
    actions =
      case {caps, stream_ended?} do
        {nil, _eos} ->
          []

        {caps, true} ->
          [caps: {pad, caps}, end_of_stream: pad]

        {caps, false} ->
          [caps: {pad, caps}]
      end

    {{:ok, actions}, state}
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

  @impl true
  def handle_other(:end_processing, ctx, state) do
    track_id =
      case ctx.name do
        {:raw_format_tee, track_id} -> track_id
        {:tee, track_id} -> track_id
      end

    {{:ok, notify: %Membrane.RTC.Engine.Event.EndProcessing{track_id: track_id}}, state}
  end
end
