defmodule Membrane.TelemetryMetrics.Utils do
  @moduledoc false

  @cleanup_event_prefix :__membrane_telemetrymetrics_cleanup__

  @spec attach_metric_handler(:telemetry.event_name(), :telemetry.handler_function(), %{
          ets: :ets.tid() | atom()
        }) :: [reference()]
  def attach_metric_handler(event_name, handler_function, %{ets: ets} = config) do
    handler_id = make_ref()
    :telemetry.attach(handler_id, event_name, handler_function, config)

    cleanup_handler_id = make_ref()

    :telemetry.attach(
      cleanup_handler_id,
      cleanup_event_name(event_name),
      &__MODULE__.handle_ets_cleanup/4,
      %{ets: ets}
    )

    [handler_id, cleanup_handler_id]
  end

  @spec cleanup_event_name(:telemetry.event_name()) :: :telemetry.event_name()
  def cleanup_event_name(base_event_name) do
    [@cleanup_event_prefix | base_event_name]
  end

  @spec handle_ets_cleanup(
          :telemetry.event_name(),
          :telemetry.event_measurements(),
          :telemetry.event_metadata(),
          %{ets: :ets.tid()}
        ) :: :ok
  def handle_ets_cleanup(_event_name, _mesaurements, metadata, %{ets: ets}) do
    with %{membrane_telemetrymetrics_label: label} <- metadata do
      :ets.delete(ets, label)
    end

    :ok
  end
end
