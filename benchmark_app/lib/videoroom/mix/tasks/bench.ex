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

    task = run_scenario()
    ret = bench()
    Task.await(task, :infinity)
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
    Mix.shell().info("Running scenario")
    mustang_options = %{target_url: "http://localhost:4000/?room_id=test"}
    options = %{count: 2, delay: 1000}
    Task.async(fn -> Stampede.start({ExampleMustang, mustang_options}, options) end)
  end

  defp bench() do
    # get schedulers type
    Mix.shell().info("Benching")
    {_, schedulers_sample} = :scheduler.sample_all()
    schedulers_type = Map.new(schedulers_sample, fn {type, id, _, _} -> {id, type} end)

    :recon.scheduler_usage(20000)
    |> Enum.map(fn {id, usage} -> {id, Map.get(schedulers_type, id), usage} end)
  end

  defp save_to_file(results) do
    File.mkdir_p!(@new)
    out = Path.join(@new, @results_file_name)
    results = :erlang.term_to_binary(results)
    File.write!(out, results)
    :noop
  end

  defp print(new) do
    print_system_info()

    base =
      if File.exists?(@base) do
        File.read!(Path.join(@base, @results_file_name))
        |> :erlang.binary_to_term()
      end

    if base do
      """
      ================
      BASE
      ================
      """
      |> Mix.shell().info()

      for row <- base do
        Mix.shell().info(format(row))
      end

      Mix.shell().info("\n")
    end

    new = if base, do: compare(base, new), else: new

    """
    ================
    NEW
    ================
    """
    |> Mix.shell().info()

    for row <- new do
      Mix.shell().info(format(row))
    end
  end

  defp compare(base, new) do
    for i <- 0..(length(new) - 1), into: [] do
      {_, _type, i_base} = Enum.at(base, i)
      {_, type, i_new} = Enum.at(new, i)
      diff = i_new - i_base
      {i, type, i_new, diff}
    end
  end

  defp format({scheduler, type, usage, diff}) do
    color = if diff < 0, do: IO.ANSI.red(), else: IO.ANSI.green()
    "#{scheduler} #{type}: #{usage} #{color}#{diff}#{IO.ANSI.reset()}"
  end

  defp format({scheduler, type, usage}) do
    "#{scheduler} #{type}: #{usage}"
  end
end
