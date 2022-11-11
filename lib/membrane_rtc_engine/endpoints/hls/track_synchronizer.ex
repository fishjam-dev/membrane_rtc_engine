defmodule Membrane.RTC.Engine.Endpoint.HLS.TrackSynchronizer do
  @moduledoc """
  Element responsible for synchronizing audio and video tracks.
  It will drop all of the incoming buffers until both pads are added and change timestamps accordingly.
  This element will synchronize only two tracks (one audio and one video).
  """
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
    {:ok, %{caps: %{}, video_first_pts: nil, input_pads_counter: 0, first_keyframe?: false}}
  end

  @impl true
  def handle_caps(Pad.ref(:input, type) = _pad, caps, ctx, state) do
    if Map.has_key?(ctx.pads, Pad.ref(:output, type)) do
      {{:ok, caps: {Pad.ref(:output, type), caps}}, state}
    else
      state = put_in(state, [:caps, type], caps)
      {:ok, state}
    end
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, _type) = _pad, _context, state) do
    {:ok, %{state | input_pads_counter: state.input_pads_counter + 1}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, type) = pad, _ctx, state)
      when is_map_key(state.caps, type) do
    caps = Map.get(state.caps, type)
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
        %{input_pads_counter: 2, first_keyframe?: true} = state
      ) do
    {{:ok, buffer: {Pad.ref(:output, :audio), buffer}}, state}
  end

  @impl true
  def handle_process(
        Pad.ref(:input, :video) = _pad,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{input_pads_counter: 2, first_keyframe?: true} = state
      ) do
    synchronized_pts = Ratio.sub(buffer.pts, state.video_first_pts)
    buffer = %Buffer{buffer | pts: synchronized_pts}
    {{:ok, buffer: {Pad.ref(:output, :video), buffer}}, state}
  end

  # first video buffer that we forward has to be a keyframe
  @impl true
  def handle_process(
        Pad.ref(:input, :video) = _pad,
        %Membrane.Buffer{} = buffer,
        _ctx,
        %{input_pads_counter: 2} = state
      ) do
    case buffer.metadata do
      %{is_keyframe: true} ->
        state = Map.put(state, :video_first_pts, buffer.pts) |> Map.put(:first_keyframe?, true)
        synchronized_pts = Ratio.sub(buffer.pts, state.video_first_pts)
        buffer = %Buffer{buffer | pts: synchronized_pts}
        {{:ok, buffer: {Pad.ref(:output, :video), buffer}}, state}

      _not_keyframe ->
        {:ok, state}
    end
  end

  @impl true
  def handle_process(_pad, _buffer, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_end_of_stream(Pad.ref(:input, type) = _pad, _ctx, state) do
    {{:ok, end_of_stream: Pad.ref(:output, type)}, state}
  end
end
