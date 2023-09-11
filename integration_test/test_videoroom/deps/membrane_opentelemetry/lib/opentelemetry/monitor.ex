defmodule Membrane.OpenTelemetry.Monitor do
  @moduledoc false

  require OpenTelemetry.Tracer

  alias Membrane.OpenTelemetry.ETSUtils

  @pdict_key :__membrane_opentelemetry_monitor__

  @spec ensure_monitor_started() :: :ok
  def ensure_monitor_started() do
    with nil <- Process.get(@pdict_key) do
      self()
      |> start()
      |> then(&Process.put(@pdict_key, &1))
    end

    :ok
  end

  @spec start(pid() | atom()) :: pid()
  def start(observed_process) do
    Process.spawn(__MODULE__, :run, [observed_process], [])
  end

  @spec run(pid() | atom()) :: :ok
  def run(observed_process) do
    Process.monitor(observed_process)

    receive do
      {:DOWN, _ref, _process, _pid, _reason} ->
        ETSUtils.get_process_spans(observed_process)
        |> Enum.each(fn span ->
          OpenTelemetry.Tracer.set_current_span(span)
          OpenTelemetry.Tracer.end_span()
        end)

        ETSUtils.delete_process_spans(observed_process)
    end

    :ok
  end
end
