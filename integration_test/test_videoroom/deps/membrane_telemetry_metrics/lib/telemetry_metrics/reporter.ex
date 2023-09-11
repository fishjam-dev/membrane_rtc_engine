defmodule Membrane.TelemetryMetrics.Reporter do
  @moduledoc """
  Attaches handlers to :telemetry events based on the received list of metrics definitions.
  The attached handlers store metrics values in ETS tables.
  These values can be gotten by calling `scrape/2` function or also reset by calling `scrape_and_cleanup/2`.

  Currently supported types of metrics are:
   - `Telemetry.Metrics.Counter`
   - `Telemetry.Metrics.Sum`
   - `Telemetry.Metrics.LastValue`

  Currently supported fields of metrics definitions are: `:name`, `:event_name`, `measurement`.
  Fields `:keep`, `:reporter_options`, `tag_values`, `tags`, `:unit` and functionalities related to them are not supported yet.
  Metrics values are grouped by `label`.
  """

  use GenServer

  @type reporter() :: pid() | atom()
  @type report() :: map()

  @spec start_link([Telemetry.Metrics.t()], GenServer.options()) :: GenServer.on_start()
  def start_link(init_arg, options \\ []) do
    options = Keyword.put(options, :trap_exit, true)
    GenServer.start_link(__MODULE__, init_arg, options)
  end

  @spec scrape(reporter(), non_neg_integer()) :: report()
  def scrape(reporter, timeout \\ 5000) do
    GenServer.call(reporter, :scrape, timeout)
  end

  @spec scrape_and_cleanup(reporter(), non_neg_integer()) :: report()
  def scrape_and_cleanup(reporter, timeout \\ 5000) do
    GenServer.call(reporter, :scrape_and_cleanup, timeout)
  end

  @spec stop(reporter()) :: :ok
  def stop(reporter) do
    GenServer.stop(reporter)
  end

  @spec child_spec(Keyword.t()) :: Supervisor.child_spec()
  def child_spec(arg) do
    {metrics, process_opts} = Keyword.pop(arg, :metrics, [])

    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [metrics, process_opts]}
    }
  end

  @impl true
  def init(metrics) do
    metrics_data =
      metrics
      |> Enum.map(fn metric ->
        ets_table = create_ets_table()
        handler_ids = attach_handlers(metric, ets_table)

        %{
          metric: metric,
          name: Enum.join(metric.name, ".") |> String.to_atom(),
          ets_table: ets_table,
          handler_ids: handler_ids
        }
      end)

    {:ok, %{metrics_data: metrics_data}}
  end

  @impl true
  def handle_call(:scrape, _from, state) do
    report =
      Enum.map(state.metrics_data, fn metric_data ->
        {metric_data.name, get_metric_report(metric_data.ets_table)}
      end)
      |> Enum.map(&move_metric_down_in_report/1)
      |> merge_metrics_reports()

    {:reply, report, state}
  end

  @impl true
  def handle_call(:scrape_and_cleanup, _from, state) do
    report =
      Enum.map(state.metrics_data, fn metric_data ->
        {metric_data.name, get_metric_report_and_do_clanup(metric_data.ets_table)}
      end)
      |> Enum.map(&move_metric_down_in_report/1)
      |> merge_metrics_reports()

    {:reply, report, state}
  end

  @impl true
  def terminate(_reason, state) do
    Enum.each(state.metrics_data, fn metric_data ->
      %{
        handler_ids: handler_ids,
        ets_table: ets_table
      } = metric_data

      Enum.each(handler_ids, &:telemetry.detach/1)
      :ets.delete(ets_table)
    end)
  end

  defp create_ets_table(),
    do: :ets.new(:metric_table, [:public, :set, {:write_concurrency, true}])

  defp attach_handlers(metric, ets_table) do
    case metric do
      %Telemetry.Metrics.Counter{} -> __MODULE__.Counter
      %Telemetry.Metrics.LastValue{} -> __MODULE__.LastValue
      %Telemetry.Metrics.Sum{} -> __MODULE__.Sum
    end
    |> apply(:attach, [metric, ets_table])
  end

  defp get_metric_report(ets_table) do
    :ets.tab2list(ets_table)
    |> aggregate_report()
  end

  defp get_metric_report_and_do_clanup(ets_table) do
    :ets.tab2list(ets_table)
    |> Enum.flat_map(fn {key, _val} -> :ets.take(ets_table, key) end)
    |> aggregate_report()
  end

  defp aggregate_report(content) do
    {aggregated_content, content_to_aggregate} =
      Enum.split_with(content, fn {key, _val} -> key == [] end)

    content_to_aggregate
    |> Enum.group_by(
      # key fun
      fn {[head | _tail], _val} -> head end,
      # value fun
      fn {[_head | tail], val} -> {tail, val} end
    )
    |> Enum.map(fn {key, subcontent} -> {key, aggregate_report(subcontent)} end)
    |> Enum.concat(aggregated_content)
  end

  defp move_metric_down_in_report({metric_name, report}) do
    Enum.map(report, fn
      {[], val} -> {metric_name, val}
      {key, sub_report} -> {key, move_metric_down_in_report({metric_name, sub_report})}
    end)
  end

  defp to_deep_map(report) when is_list(report) do
    Map.new(report, fn {key, val} -> {key, to_deep_map(val)} end)
  end

  defp to_deep_map(report), do: report

  defp merge_metrics_reports(reports) do
    Enum.map(reports, &to_deep_map/1)
    |> Enum.reduce(%{}, &merge_metrics_reports/2)
  end

  defp merge_metrics_reports(report1, report2) do
    Map.merge(
      report1,
      report2,
      fn _key, val1, val2 -> merge_metrics_reports(val1, val2) end
    )
  end
end
