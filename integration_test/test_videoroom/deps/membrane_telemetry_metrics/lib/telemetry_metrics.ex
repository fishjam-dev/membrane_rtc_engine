defmodule Membrane.TelemetryMetrics do
  @moduledoc """
  Defines macros for executing telemetry events and registering processes with events.
  Provided macros evalueates to meaningful code or to nothing, depending on config values, in order to achieve performance boost, when specific event or whole telemetry is not in use.
  """

  @enabled Application.compile_env(:membrane_telemetry_metrics, :enabled, false)
  @events Application.compile_env(:membrane_telemetry_metrics, :events, :all)

  @type label() :: list()

  @doc """
  Evaluates to conditional call to `:telemetry.execute/3` or to nothing, depending on if specific event is enabled in config file.
  If event is enabled, `:telemetry.execute/3` will be executed only if value returned by call to `func` will be truthly.
  """
  defmacro conditional_execute(func, event_name, measurements, metadata, label) do
    if emit_event?(event_name) do
      quote do
        if unquote(func).() do
          metadata = Map.put(unquote(metadata), :membrane_telemetrymetrics_label, unquote(label))

          :telemetry.execute(
            unquote(event_name),
            unquote(measurements),
            metadata
          )
        end
      end
    else
      quote do
        fn ->
          _unused = unquote(func)
          _unused = unquote(event_name)
          _unused = unquote(measurements)
          _unused = unquote(metadata)
          _unused = unquote(label)
        end
      end
    end
  end

  @doc """
  Evaluates to call to `:telemetry.execute/3` or to nothing, depending on if specific event is enabled in config file.
  """
  defmacro execute(event_name, measurments, metadata, label) do
    if emit_event?(event_name) do
      quote do
        metadata = Map.put(unquote(metadata), :membrane_telemetrymetrics_label, unquote(label))

        :telemetry.execute(
          unquote(event_name),
          unquote(measurments),
          metadata
        )
      end
    else
      quote do
        fn ->
          _unused = unquote(event_name)
          _unused = unquote(measurments)
          _unused = unquote(metadata)
          _unused = unquote(label)
        end
      end
    end
  end

  @doc """
  Evalueates to call to `Membrane.TelemetryMetrics.Monitor.start/3` or to nothing, depending on if specific event is enabled in config file.
  Should be called in every process, that will execute event linked with metric aggregated by some instance of `Membrane.TelemetryMetrics.Reporter`.
  """
  defmacro register(event_name, label) do
    if emit_event?(event_name) do
      quote do
        Membrane.TelemetryMetrics.Monitor.start(unquote(event_name), unquote(label))
      end
    else
      quote do
        fn ->
          _unused = unquote(event_name)
          _unused = unquote(label)
        end
      end
    end
  end

  defp emit_event?(event) do
    cond do
      not @enabled -> false
      @events == :all -> true
      is_list(@events) -> event in @events
      true -> false
    end
  end
end
