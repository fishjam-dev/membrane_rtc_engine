defmodule Membrane.RTC.Engine.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from `membrane_telemetry_metrics`.
  `Membrane.TelemetryMetrics.Reporter` started with metrics returned by `metrics/1` function will be able to generate reports, that matches type `Membrane.RTC.Engine.Metrics.rtc_engine_report()`
  You can see usage example in (`membrane_videoroom`)[github.com/membraneframework/membrane_videoroom].
  """

  @type rtc_engine_report() :: %{
          optional({:room_id, binary()}) => %{
            optional({:peer_id, binary()}) => %{
              optional({:track_id, binary()}) => %{
                "inbound-rtp.encoding" => :OPUS | :VP8 | :H264,
                "inbound-rtp.ssrc" => integer(),
                "inbound-rtp.bytes_received" => integer(),
                "inbound-rtp.keyframe_request_sent" => integer(),
                "inbound-rtp.packets" => integer(),
                "inbound-rtp.VP8.frames" => integer(),
                "inbound-rtp.VP8.keyframes" => integer(),
                "inbound-rtp.OPUS.frames" => integer(),
                "inbound-rtp.H264.keyframes" => integer()
              },
              "ice.binding_requests_received" => integer(),
              "ice.binding_responses_sent" => integer(),
              "ice.bytes_received" => integer(),
              "ice.bytes_sent" => integer(),
              "ice.packets_received" => integer(),
              "ice.packets_sent" => integer()
            }
          }
        }

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    Enum.concat([
      rtc_engine_metrics(),
      Membrane.RTP.Metrics.metrics(),
      Membrane.ICE.Metrics.metrics()
    ])
  end

  defp rtc_engine_metrics() do
    [
      Telemetry.Metrics.sum(
        "inbound-rtp.VP8.frames",
        event_name: [:packet_arrival, :rtp, :VP8],
        measurement: :frame_indicator
      ),
      Telemetry.Metrics.sum(
        "inbound-rtp.VP8.keyframes",
        event_name: [:packet_arrival, :rtp, :VP8],
        measurement: :keyframe_indicator
      ),
      Telemetry.Metrics.sum(
        "inbound-rtp.H264.keyframes",
        event_name: [:packet_arrival, :rtp, :H264],
        measurement: :keyframe_indicator
      ),
      Telemetry.Metrics.counter(
        "inbound-rtp.OPUS.frames",
        event_name: [:packet_arrival, :rtp, :OPUS]
      )
    ]
  end
end
