defmodule Mix.Tasks.Bench do
  @moduledoc """
  Benchmark RTC Engine performance
  """
  use Mix.Task
  use Bunch

  require Membrane.Logger

  @requirements ["app.start"]

  @out "benchmark"
  @base Path.join([@out, "base"])
  @new Path.join([@out, "new"])
  @results_file_name "results"

  @impl Mix.Task
  def run(_args) do
    run_scenario()
    ret = bench()
    print(ret)
    save_to_file(ret)
    Mix.shell().info("Results successfully saved to #{inspect(@new)} directory")
  end

  defp run_scenario() do
  end

  defp bench() do
    :recon.scheduler_usage(5000)
  end

  defp save_to_file(results) do
    if File.exists?(@new) do
      File.rm_rf!(@base)
      File.rename!(@new, @base)
    end

    File.mkdir_p!(@new)
    do_save(@new, results)
    :noop
  end

  defp do_save(directory, results) do
    out = Path.join(directory, @results_file_name)
    results = :erlang.term_to_binary(results)
    File.write!(out, results)
  end

  defp print(data) do
    Mix.shell().info("#{inspect(data)}")
  end
end
