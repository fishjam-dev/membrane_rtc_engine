defmodule Membrane.TelemetryMetrics.Monitor do
  @moduledoc false

  require Membrane.TelemetryMetrics

  alias Membrane.TelemetryMetrics.Utils

  @spec start(:telemetry.event_name(), Membrane.TelemetryMetrics.label()) :: {:ok, pid()}
  def start(event_name, label) do
    pid =
      Process.spawn(
        __MODULE__,
        :run,
        [self(), event_name, label],
        []
      )

    {:ok, pid}
  end

  @spec run(pid() | atom(), :telemetry.event_name(), Membrane.TelemetryMetrics.label()) :: :ok
  def run(monitored_process, event_name, label) do
    Process.monitor(monitored_process)

    receive do
      {:DOWN, _ref, _process, ^monitored_process, _reason} ->
        Utils.cleanup_event_name(event_name)
        |> Membrane.TelemetryMetrics.execute(%{}, %{}, label)
    end

    :ok
  end
end
