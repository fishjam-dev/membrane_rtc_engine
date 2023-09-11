defmodule Membrane.TelemetryMetrics.Reporter.Counter do
  @moduledoc false

  alias Membrane.TelemetryMetrics.Utils

  @spec attach(Telemetry.Metrics.Counter.t(), :ets.tid() | atom()) :: [reference()]
  def attach(metric, ets) do
    Utils.attach_metric_handler(metric.event_name, &__MODULE__.handle_event/4, %{ets: ets})
  end

  @spec handle_event(
          :telemetry.event_name(),
          :telemetry.event_measurements(),
          :telemetry.event_metadata(),
          %{ets: :ets.tid() | atom()}
        ) :: :ok
  def handle_event(_event_name, _measurements, metadata, %{ets: ets}) do
    with %{membrane_telemetrymetrics_label: label} <- metadata do
      :ets.update_counter(ets, label, 1, {label, 0})
    end

    :ok
  end
end
