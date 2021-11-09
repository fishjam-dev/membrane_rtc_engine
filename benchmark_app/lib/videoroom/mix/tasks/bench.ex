defmodule Mix.Tasks.Bench do
  @moduledoc """
  Benchmark RTC Engine performance
  """
  use Mix.Task

  @requirements ["app.start"]

  @impl Mix.Task
  def run(_args) do
    run_scenario()
    ret = bench()
    save_to_file(ret)
    print(ret)
  end

  defp run_scenario() do
  end

  defp bench() do
    :recon.scheduler_usage(5000)
  end

  defp save_to_file(_data) do
  end

  defp print(data) do
    IO.inspect(data)
  end
end
