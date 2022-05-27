defmodule Membrane.RTC.Engine.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from `membrane_telemetry_metrics`.
  You can see usage example in (`VideoRoom.Application`)[https://github.com/membraneframework/membrane_videoroom/blob/master/lib/videoroom/application.ex].
  `Membrane.TelemetryMetrics.Reporter` started with metrics returned by metrics/1 function will be able to generate reports like this one below:

  ```elixir
  %{
    {:room_id, "daily_meeting"} => %{
      {:peer_id, "32bfdd9b-916f-4a06-980a-5961a157b858"} => %{
        {:track_id,
        "32bfdd9b-916f-4a06-980a-5961a157b858:486dddfe-2c83-4637-a801-a7e8c950fac5:l"} => %{
          "inbound-rtp.VP8.frames" => 1430,
          "inbound-rtp.VP8.keyframes" => 7,
          "inbound-rtp.bytes_received" => 1039741,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 5,
          "inbound-rtp.packets" => 1519,
          "inbound-rtp.ssrc" => 4108675401
        },
        {:track_id,
        "32bfdd9b-916f-4a06-980a-5961a157b858:486dddfe-2c83-4637-a801-a7e8c950fac5:m"} => %{
          "inbound-rtp.VP8.frames" => 1423,
          "inbound-rtp.VP8.keyframes" => 6,
          "inbound-rtp.bytes_received" => 5329455,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 5,
          "inbound-rtp.packets" => 5101,
          "inbound-rtp.ssrc" => 1150647132
        },
        {:track_id,
        "32bfdd9b-916f-4a06-980a-5961a157b858:d88da4bc-8d29-4ba2-a179-28a5f3b530b7:"} => %{
          "inbound-rtp.OPUS.frames" => 2987,
          "inbound-rtp.bytes_received" => 313779,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 5,
          "inbound-rtp.packets" => 2987,
          "inbound-rtp.ssrc" => 3787421452
        },
        "ice.binding_requests_received" => 27,
        "ice.binding_responses_sent" => 27,
        "ice.bytes_received" => 6715001,
        "ice.bytes_sent" => 5684471,
        "ice.packets_received" => 10145,
        "ice.packets_sent" => 8452
      }
    }
  ```
  """

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
