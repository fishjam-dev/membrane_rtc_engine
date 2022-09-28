defmodule Membrane.RTC.Engine.TrackSynchronizer do
  use Membrane.Filter

  alias Membrane.Buffer

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request,
    demand_mode: :auto

  def_output_pad :output,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request,
    demand_mode: :auto

  @impl true
  def handle_init(_opts) do
    {:ok, %{caps: %{}, video_first_pts: 0, input_pads_counter: 0}}
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
  def handle_process(
        Pad.ref(:input, :audio) = _pad,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{input_pads_counter: 2} = state
      ) do
    {{:ok, buffer: {Pad.ref(:output, :audio), buffer}}, state}
  end

  @impl true
  def handle_process(
        Pad.ref(:input, :video) = _pad,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{input_pads_counter: 2} = state
      ) do
    state =
      if 0 == Map.get(state, :video_first_pts),
        do: Map.put(state, :video_first_pts, buffer.pts),
        else: state

    buffer = %Buffer{buffer | pts: Ratio.sub(buffer.pts, state.video_first_pts)}
    {{:ok, buffer: {Pad.ref(:output, :video), buffer}}, state}
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    IO.inspect(buffer, label: :else)
    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, type) = _pad, _ctx, state) do
    {{:ok, end_of_stream: Pad.ref(:output, type)}, state}
  end
end
