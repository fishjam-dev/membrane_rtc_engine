defmodule Membrane.RTC.Engine.Endpoint.Recording.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from
  [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics).
  You can see usage example in [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom).

  See also `Metrics` guide for more details and example report that you can obtain with
  `Membrane.TelemetryMetrics.Reporter`.
  """

  require Membrane.TelemetryMetrics

  @upload_event [Membrane.RTC.Engine.Endpoint.Recording, :S3]

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    [
      Telemetry.Metrics.counter(
        "recording.s3-upload",
        event_name: @upload_event,
        tags: [:status]
      )
    ]
  end

  @doc false
  @spec register_s3_upload_events(Membrane.TelemetryMetrics.label()) :: :ok
  def register_s3_upload_events(telemetry_label) do
    Membrane.TelemetryMetrics.register(@upload_event, telemetry_label)
    :ok
  end

  @doc false
  @spec emit_upload_completed_event(Membrane.TelemetryMetrics.label()) :: :ok
  def emit_upload_completed_event(telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @upload_event,
      %{},
      %{status: :completed},
      telemetry_label
    )

    :ok
  end

  @doc false
  @spec emit_upload_aborted_event(Membrane.TelemetryMetrics.label()) :: :ok
  def emit_upload_aborted_event(telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @upload_event,
      %{},
      %{status: :aborted},
      telemetry_label
    )

    :ok
  end
end
