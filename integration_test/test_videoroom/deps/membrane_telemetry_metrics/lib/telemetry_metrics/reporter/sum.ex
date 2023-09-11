defmodule Membrane.TelemetryMetrics.Reporter.Sum do
  @moduledoc false

  alias Membrane.TelemetryMetrics.Utils

  @spec attach(Telemetry.Metrics.Sum.t(), :ets.tid() | atom()) :: [reference()]
  def attach(metric, ets) do
    config = %{ets: ets, measurement: metric.measurement}
    Utils.attach_metric_handler(metric.event_name, &__MODULE__.handle_event/4, config)
  end

  @spec handle_event(
          :telemetry.event_name(),
          :telemetry.event_measurements(),
          :telemetry.event_metadata(),
          %{ets: :ets.tid() | atom(), measurement: atom()}
        ) :: :ok
  def handle_event(_event_name, measurements, metadata, config) do
    %{ets: ets, measurement: measurement} = config

    with %{membrane_telemetrymetrics_label: label} <- metadata,
         %{^measurement => value} <- measurements do
      :ets.update_counter(ets, label, value, {label, 0})
    end

    :ok
  end
end
