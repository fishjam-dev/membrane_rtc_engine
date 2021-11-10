defmodule Mix.Tasks.Bench.Clean do
  @moduledoc """
  Clean output of `mix bench`
  """
  use Mix.Task

  @out "benchmark"

  @impl Mix.Task
  def run(_args) do
    File.rm_rf!(@out)
  end
end
