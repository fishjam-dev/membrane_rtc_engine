defmodule Membrane.RTC.Engine.Endpoint.Recording.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from
  [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics).
  You can see usage example in [`jellyfish`](https://github.com/jellyfish-dev/jellyfish).

  See also `Metrics` guide for more details and example report that you can obtain with
  `Membrane.TelemetryMetrics.Reporter`.
  """

  require Membrane.TelemetryMetrics

  @upload_event [Membrane.RTC.Engine.Endpoint.Recording, :S3]

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    [
      Telemetry.Metrics.counter(
        "recording.s3.upload.completed",
        event_name: @upload_event ++ [:completed]
      ),
      Telemetry.Metrics.counter(
        "recording.s3.upload.aborted",
        event_name: @upload_event ++ [:aborted]
      )
    ]
  end

  @doc false
  @spec emit_completed_upload_event(Membrane.TelemetryMetrics.label()) :: :ok
  def emit_completed_upload_event(telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @upload_event ++ [:completed],
      %{},
      %{},
      telemetry_label
    )

    :ok
  end

  @doc false
  @spec emit_aborted_upload_event(Membrane.TelemetryMetrics.label()) :: :ok
  def emit_aborted_upload_event(telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @upload_event ++ [:aborted],
      %{},
      %{},
      telemetry_label
    )

    :ok
  end
end
