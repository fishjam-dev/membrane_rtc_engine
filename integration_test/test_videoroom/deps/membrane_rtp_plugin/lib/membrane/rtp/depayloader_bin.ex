defmodule Membrane.RTP.DepayloaderBin do
  @moduledoc """
  Modules responsible for reordering incoming RTP packets using a jitter buffer
  to later depayload packet's payload from RTP format.
  """

  use Membrane.Bin

  alias Membrane.RTP
  alias Membrane.RTP.JitterBuffer

  def_options depayloader: [
                spec: module(),
                description: "Depayloader module that should be used for incoming stream"
              ],
              clock_rate: [
                spec: RTP.clock_rate_t()
              ]

  def_input_pad :input,
    accepted_format: RTP,
    demand_unit: :buffers

  def_output_pad :output,
    accepted_format: _any,
    demand_unit: :buffers

  @impl true
  def handle_init(_ctx, opts) do
    structure =
      bin_input()
      |> child(:jitter_buffer, %JitterBuffer{clock_rate: opts.clock_rate})
      |> child(:depayloader, opts.depayloader)
      |> bin_output()

    {[spec: structure], %{}}
  end
end
