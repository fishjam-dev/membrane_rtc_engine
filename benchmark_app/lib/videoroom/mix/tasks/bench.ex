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
    if File.exists?(@new) do
      File.rm_rf!(@base)
      File.rename!(@new, @base)
    end

    print_system_info()
    run_scenario()
    ret = bench()
    print(ret)
    save_to_file(ret)
    Mix.shell().info("Results successfully saved to #{inspect(@new)} directory")
  end

  defp print_system_info() do
    info = """

    ================
    SYSTEM INFO
    ================

    System version: #{:erlang.system_info(:system_version)}\
    System arch: #{:erlang.system_info(:system_architecture)}
    NIF version: #{:erlang.system_info(:nif_version)}
    """

    Mix.shell().info(info)
  end

  defp run_scenario() do
  end

  defp bench() do
    :recon.scheduler_usage(5000)
  end

  defp save_to_file(results) do
    File.mkdir_p!(@new)
    out = Path.join(@new, @results_file_name)
    results = :erlang.term_to_binary(results)
    File.write!(out, results)
    :noop
  end

  defp print(new) do
    base =
      if File.exists?(@base) do
        File.read!(Path.join(@base, @results_file_name))
        |> :erlang.binary_to_term()
      end

    if base do
      Mix.shell().info("Base:")

      for row <- base do
        Mix.shell().info(format(row))
      end

      Mix.shell().info("\n")
    end

    new = if base, do: compare(base, new), else: new

    Mix.shell().info("New:")

    for row <- new do
      Mix.shell().info(format(row))
    end
  end

  defp compare(base, new) do
    for i <- 0..(length(new) - 1), into: [] do
      {_, i_base} = Enum.at(base, i)
      {_, i_new} = Enum.at(new, i)
      diff = i_new - i_base
      {i, i_new, diff}
    end
  end

  defp format({scheduler, usage, diff}) do
    color = if diff < 0, do: IO.ANSI.red(), else: IO.ANSI.green()
    "#{scheduler}: #{usage} #{color}#{diff}#{IO.ANSI.reset()}"
  end

  defp format({scheduler, usage}) do
    "#{scheduler}: #{usage}"
  end
end
