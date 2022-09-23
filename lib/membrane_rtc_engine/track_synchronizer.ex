defmodule Membrane.RTC.Engine.TrackSynchronizer do
  use Membrane.Filter

  alias Membrane.Buffer

  def_input_pad :input,
    caps: [
      {Membrane.H264, stream_format: :byte_stream},
      Membrane.AAC,
      Membrane.Opus
    ],
    demand_mode: :auto,
    demand_unit: :buffers,
    availability: :on_request

  def_output_pad :output,
    caps: [
      {Membrane.H264, stream_format: :byte_stream},
      Membrane.AAC,
      Membrane.Opus
    ],
    demand_mode: :auto,
    demand_unit: :buffers,
    availability: :on_request

  @impl true
  def handle_init(_opts) do
    {:ok, %{caps: %{}, input_pads_counter: 0, dts: %{}}}
  end

  @impl true
  def handle_caps(Pad.ref(:input, type) = _pad, caps, ctx, state) do
    case Map.has_key?(ctx.pads, Pad.ref(:output, type)) do
      true ->
        {{:ok, caps: {Pad.ref(:output, type), caps}}, state}

      false ->
        state = put_in(state, [:caps, type], caps)
        {:ok, state}
    end
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, _type) = _pad, _context, state) do
    {:ok, %{state | input_pads_counter: state.input_pads_counter + 1}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, :video) = pad, _ctx, %{caps: %{video: caps}} = state) do
    {{:ok, caps: {pad, caps}}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, :audio) = pad, _ctx, %{caps: %{audio: caps}} = state) do
    {{:ok, caps: {pad, caps}}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _type) = _pad, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_process(Pad.ref(:input, type) = _pad, %Membrane.Buffer{} = buffer, _ctx, state) do
    case state.input_pads_counter do
      2 ->
        state =
          if is_nil(Map.get(state.dts, type)),
            do: put_in(state, [:dts, type], buffer.pts),
            else: state

        buffer = %Buffer{buffer | pts: Ratio.sub(buffer.pts, Map.get(state.dts, type, 0))}
        {{:ok, buffer: {Pad.ref(:output, type), buffer}}, state}

      _else ->
        {:ok, state}
    end
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, type) = _pad, _ctx, state) do
    {{:ok, end_of_stream: Pad.ref(:output, type)}, state}
  end
end
