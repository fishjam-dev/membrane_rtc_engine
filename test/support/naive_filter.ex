defmodule Membrane.RTC.Engine.Support.NaiveFilter do
  # Naive filter which only forwards packets
  use Membrane.Filter

  def_input_pad :input,
    availability: :always,
    caps: :any,
    demand_unit: :buffers,
    demand_mode: :auto

  def_output_pad :output,
    availability: :always,
    caps: :any,
    demand_mode: :auto

  @impl true
  def handle_init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, _ctx, state) do
    {{:ok, buffer: {:output, buffer}}, state}
  end
end
