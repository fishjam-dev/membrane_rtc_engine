defmodule Membrane.RTC.Engine.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from `membrane_telemetry_metrics`.
  You can see usage example in (`VideoRoom.Application`)[https://github.com/membraneframework/membrane_videoroom/blob/master/lib/videoroom/application.ex].
  `Membrane.TelemetryMetrics.Reporter` started with metrics returned by metrics/1 function will be able to generate reports like this one below:

  ```
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
      },
      {:peer_id, "a1635f0e-f52c-4254-9d4f-ab7a69c8cc11"} => %{
        {:track_id,
        "a1635f0e-f52c-4254-9d4f-ab7a69c8cc11:4195e933-b390-4a47-9e9b-31d2e357b7af:l"} => %{
          "inbound-rtp.VP8.frames" => 1540,
          "inbound-rtp.VP8.keyframes" => 9,
          "inbound-rtp.bytes_received" => 1125831,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 6,
          "inbound-rtp.packets" => 1650,
          "inbound-rtp.ssrc" => 1214983820
        },
        {:track_id,
        "a1635f0e-f52c-4254-9d4f-ab7a69c8cc11:4195e933-b390-4a47-9e9b-31d2e357b7af:m"} => %{
          "inbound-rtp.VP8.frames" => 1533,
          "inbound-rtp.VP8.keyframes" => 8,
          "inbound-rtp.bytes_received" => 5747531,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 6,
          "inbound-rtp.packets" => 5535,
          "inbound-rtp.ssrc" => 2221261902
        },
        {:track_id,
        "a1635f0e-f52c-4254-9d4f-ab7a69c8cc11:bcde16ce-4192-4341-bf9b-e573c5933bea:"} => %{
          "inbound-rtp.OPUS.frames" => 3225,
          "inbound-rtp.bytes_received" => 327717,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 6,
          "inbound-rtp.packets" => 3225,
          "inbound-rtp.ssrc" => 2054950190
        },
        "ice.binding_requests_received" => 33,
        "ice.binding_responses_sent" => 33,
        "ice.bytes_received" => 7268971,
        "ice.bytes_sent" => 5632112,
        "ice.packets_received" => 11919,
        "ice.packets_sent" => 8355
      }
    },
    {:room_id, "technical_interview"} => %{
      {:peer_id, "ae42e869-5ecb-47b0-8956-f1390a4f8283"} => %{
        {:track_id,
        "ae42e869-5ecb-47b0-8956-f1390a4f8283:46da3c72-1e60-4029-9b22-bff9b28e9405:"} => %{
          "inbound-rtp.OPUS.frames" => 2163,
          "inbound-rtp.bytes_received" => 221643,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 2163,
          "inbound-rtp.ssrc" => 3893001048
        },
        {:track_id,
        "ae42e869-5ecb-47b0-8956-f1390a4f8283:9f287163-c174-46b0-84b9-31d6b90e7029:h"} => %{
          "inbound-rtp.VP8.frames" => 85,
          "inbound-rtp.VP8.keyframes" => 2,
          "inbound-rtp.bytes_received" => 527060,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.packets" => 478,
          "inbound-rtp.ssrc" => 3705327476
        },
        {:track_id,
        "ae42e869-5ecb-47b0-8956-f1390a4f8283:9f287163-c174-46b0-84b9-31d6b90e7029:l"} => %{
          "inbound-rtp.VP8.frames" => 905,
          "inbound-rtp.VP8.keyframes" => 5,
          "inbound-rtp.bytes_received" => 666511,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 962,
          "inbound-rtp.ssrc" => 1265024678
        },
        {:track_id,
        "ae42e869-5ecb-47b0-8956-f1390a4f8283:9f287163-c174-46b0-84b9-31d6b90e7029:m"} => %{
          "inbound-rtp.VP8.frames" => 915,
          "inbound-rtp.VP8.keyframes" => 5,
          "inbound-rtp.bytes_received" => 3773921,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 3556,
          "inbound-rtp.ssrc" => 1916623047
        },
        "ice.binding_requests_received" => 21,
        "ice.binding_responses_sent" => 21,
        "ice.bytes_received" => 5212479,
        "ice.bytes_sent" => 4162929,
        "ice.packets_received" => 7562,
        "ice.packets_sent" => 6186
      },
      {:peer_id, "d0285bcd-f77f-4075-9fed-4c602bc68842"} => %{
        {:track_id,
        "d0285bcd-f77f-4075-9fed-4c602bc68842:0536a203-7ea7-4b15-b857-5f34b855cde7:l"} => %{
          "inbound-rtp.VP8.frames" => 1134,
          "inbound-rtp.VP8.keyframes" => 7,
          "inbound-rtp.bytes_received" => 812627,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 1221,
          "inbound-rtp.ssrc" => 3862273492
        },
        {:track_id,
        "d0285bcd-f77f-4075-9fed-4c602bc68842:0536a203-7ea7-4b15-b857-5f34b855cde7:m"} => %{
          "inbound-rtp.VP8.frames" => 1128,
          "inbound-rtp.VP8.keyframes" => 6,
          "inbound-rtp.bytes_received" => 4256915,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 4134,
          "inbound-rtp.ssrc" => 2664561968
        },
        {:track_id,
        "d0285bcd-f77f-4075-9fed-4c602bc68842:9c7c0ed2-b567-4b56-9528-c38011d82998:"} => %{
          "inbound-rtp.OPUS.frames" => 2375,
          "inbound-rtp.bytes_received" => 243441,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 4,
          "inbound-rtp.packets" => 2375,
          "inbound-rtp.ssrc" => 4063921578
        },
        "ice.binding_requests_received" => 26,
        "ice.binding_responses_sent" => 26,
        "ice.bytes_received" => 5362055,
        "ice.bytes_sent" => 3988571,
        "ice.packets_received" => 8825,
        "ice.packets_sent" => 5913
      }
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
