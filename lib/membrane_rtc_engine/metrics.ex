defmodule Membrane.RTC.Engine.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from 
  [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics).
  You can see usage example in [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom).

  See also `Metrics` guide for more details and example report that you can obtain with 
  `Membrane.TelemetryMetrics.Reporter`.
  """

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    Enum.concat([
      rtc_engine_metrics(),
      Membrane.RTP.Metrics.metrics(),
      Membrane.ICE.Metrics.metrics(),
      Membrane.WebRTC.Metrics.metrics()
    ])
  end

  defp rtc_engine_metrics() do
    [
      Telemetry.Metrics.sum(
        "inbound-rtp.frames",
        event_name: [Membrane.RTC.Engine, :RTP, :packet, :arrival],
        measurement: :frame_indicator
      ),
      Telemetry.Metrics.sum(
        "inbound-rtp.keyframes",
        event_name: [Membrane.RTC.Engine, :RTP, :packet, :arrival],
        measurement: :keyframe_indicator
      ),
      Telemetry.Metrics.last_value(
        "outbound-rtp.variant",
        event_name: [Membrane.RTC.Engine, :RTP, :variant, :switched],
        measurement: :variant
      ),
      Telemetry.Metrics.last_value(
        "outbound-rtp.variant-reason",
        event_name: [Membrane.RTC.Engine, :RTP, :variant, :switched],
        measurement: :reason
      ),
      Telemetry.Metrics.last_value(
        "endpoint.bandwidth",
        event_name: [Membrane.RTC.Engine, :endpoint, :bandwidth],
        measurement: :bandwidth
      ),
      Telemetry.Metrics.last_value(
        "endpoint.metadata",
        event_name: [Membrane.RTC.Engine, :endpoint, :metadata, :event],
        measurement: :metadata
      ),
      Telemetry.Metrics.last_value(
        "track.metadata",
        event_name: [Membrane.RTC.Engine, :track, :metadata, :event],
        measurement: :metadata
      )
    ]
  end
end
