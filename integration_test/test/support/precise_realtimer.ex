defmodule Membrane.RTC.Engine.PreciseRealtimer do
  @moduledoc false

  use Membrane.Filter

  def_input_pad(:input, accepted_format: _any)
  def_output_pad(:output, accepted_format: _any)

  def_options(
    timer_resolution: [
      spec: Membrane.Time.t()
    ]
  )

  @impl true
  def handle_init(_ctx, opts) do
    {[],
     %{
       tick_idx: 0,
       resolution: opts.timer_resolution,
       queue: Qex.new()
     }}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[start_timer: {:timer, state.resolution}], state}
  end

  @impl true
  def handle_buffer(:input, buffer, _ctx, state) do
    {[], %{state | queue: Qex.push(state.queue, {:buffer, {:output, buffer}})}}
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    {[], %{state | queue: Qex.push(state.queue, {:end_of_stream, :output})}}
  end

  @impl true
  def handle_tick(:timer, _ctx, %{tick_idx: tick_idx} = state) do
    current_ts = tick_idx * state.resolution
    state = %{state | tick_idx: tick_idx + 1}

    if Enum.empty?(state.queue) do
      {[], state}
    else
      {actions, buffered} =
        Enum.split_while(state.queue, fn
          {:buffer, {:output, buffer}} ->
            Membrane.Buffer.get_dts_or_pts(buffer) <= current_ts

          _other ->
            true
        end)

      {actions, %{state | queue: Qex.new(buffered)}}
    end
  end
end
