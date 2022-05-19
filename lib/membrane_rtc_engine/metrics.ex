defmodule Membrane.RTC.Engine.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from `membrane_telemetry_metrics`.
  You can see usage example in (`VideoRoom.Application`)[https://github.com/membraneframework/membrane_videoroom/blob/master/lib/videoroom/application.ex].
  `Membrane.TelemetryMetrics.Reporter` started with metrics returned by metrics/1 function will be able to generate reports like this one below:

  ```
  %{
    {:room_id, "daily_meeting"} => %{
      {:peer_id, "9d9a632d-3d46-4908-b040-bebe63ec6d19"} => %{
        {:track_id,
        "9d9a632d-3d46-4908-b040-bebe63ec6d19:7553bc03-e0c4-4a1f-9cf7-0f9e923d33ae:"} => %{
          "inbound-rtp.OPUS.frames" => 8859,
          "inbound-rtp.bytes_received" => 850978,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 8859,
          "inbound-rtp.ssrc" => 312037136
        },
        {:track_id,
        "9d9a632d-3d46-4908-b040-bebe63ec6d19:7e803d01-bf0c-435c-bc80-fe39f586c202:l"} => %{
          "inbound-rtp.VP8.frames" => 4248,
          "inbound-rtp.VP8.keyframes" => 20,
          "inbound-rtp.bytes_received" => 3112012,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 4460,
          "inbound-rtp.ssrc" => 2348844088
        },
        {:track_id,
        "9d9a632d-3d46-4908-b040-bebe63ec6d19:7e803d01-bf0c-435c-bc80-fe39f586c202:m"} => %{
          "inbound-rtp.VP8.frames" => 4243,
          "inbound-rtp.VP8.keyframes" => 19,
          "inbound-rtp.bytes_received" => 16819092,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 16015,
          "inbound-rtp.ssrc" => 1844234322
        }
      },
      {:peer_id, "c557f2f2-f42c-406e-823a-233cd1158fde"} => %{
        {:track_id,
        "c557f2f2-f42c-406e-823a-233cd1158fde:090d83a9-d050-4c71-9de5-83e9e3fa2db4:"} => %{
          "inbound-rtp.OPUS.frames" => 8520,
          "inbound-rtp.bytes_received" => 894702,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 8520,
          "inbound-rtp.ssrc" => 814952113
        },
        {:track_id,
        "c557f2f2-f42c-406e-823a-233cd1158fde:76cf884d-4f69-4f53-b1f2-143d269f7527:l"} => %{
          "inbound-rtp.VP8.frames" => 4089,
          "inbound-rtp.VP8.keyframes" => 19,
          "inbound-rtp.bytes_received" => 3037526,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 4312,
          "inbound-rtp.ssrc" => 977746364
        },
        {:track_id,
        "c557f2f2-f42c-406e-823a-233cd1158fde:76cf884d-4f69-4f53-b1f2-143d269f7527:m"} => %{
          "inbound-rtp.VP8.frames" => 4083,
          "inbound-rtp.VP8.keyframes" => 18,
          "inbound-rtp.bytes_received" => 16342230,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 17,
          "inbound-rtp.packets" => 15470,
          "inbound-rtp.ssrc" => 3945285486
        }
      }
    },
    {:room_id, "technical_interview"} => %{
      {:peer_id, "bd222d11-cb39-47d5-8923-2cee951cb315"} => %{
        {:track_id,
        "bd222d11-cb39-47d5-8923-2cee951cb315:f261f378-b372-43c8-a475-197f991c844f:"} => %{
          "inbound-rtp.OPUS.frames" => 7031,
          "inbound-rtp.bytes_received" => 734163,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 7031,
          "inbound-rtp.ssrc" => 3491539918
        },
        {:track_id,
        "bd222d11-cb39-47d5-8923-2cee951cb315:ffc70c38-c864-4d2b-8ff3-694a25f467c5:h"} => %{
          "inbound-rtp.VP8.frames" => 2288,
          "inbound-rtp.VP8.keyframes" => 20,
          "inbound-rtp.bytes_received" => 18396438,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 9,
          "inbound-rtp.packets" => 16313,
          "inbound-rtp.ssrc" => 1290379009
        },
        {:track_id,
        "bd222d11-cb39-47d5-8923-2cee951cb315:ffc70c38-c864-4d2b-8ff3-694a25f467c5:l"} => %{
          "inbound-rtp.VP8.frames" => 1051,
          "inbound-rtp.VP8.keyframes" => 6,
          "inbound-rtp.bytes_received" => 769436,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 1103,
          "inbound-rtp.ssrc" => 2168821977
        },
        {:track_id,
        "bd222d11-cb39-47d5-8923-2cee951cb315:ffc70c38-c864-4d2b-8ff3-694a25f467c5:m"} => %{
          "inbound-rtp.VP8.frames" => 1065,
          "inbound-rtp.VP8.keyframes" => 6,
          "inbound-rtp.bytes_received" => 4293386,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 4060,
          "inbound-rtp.ssrc" => 2213291804
        }
      },
      {:peer_id, "e89038c4-ac21-4c2d-bea1-1ed605cfba0f"} => %{
        {:track_id,
        "e89038c4-ac21-4c2d-bea1-1ed605cfba0f:5e66b21c-c825-4ede-8f31-6ac7d3027c83:l"} => %{
          "inbound-rtp.VP8.frames" => 3485,
          "inbound-rtp.VP8.keyframes" => 17,
          "inbound-rtp.bytes_received" => 2571619,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 3682,
          "inbound-rtp.ssrc" => 129978023
        },
        {:track_id,
        "e89038c4-ac21-4c2d-bea1-1ed605cfba0f:5e66b21c-c825-4ede-8f31-6ac7d3027c83:m"} => %{
          "inbound-rtp.VP8.frames" => 3478,
          "inbound-rtp.VP8.keyframes" => 16,
          "inbound-rtp.bytes_received" => 13630727,
          "inbound-rtp.encoding" => :VP8,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 12989,
          "inbound-rtp.ssrc" => 1330927960
        },
        {:track_id,
        "e89038c4-ac21-4c2d-bea1-1ed605cfba0f:784a3b30-1007-4f88-b8df-b76a5f29d737:"} => %{
          "inbound-rtp.OPUS.frames" => 7264,
          "inbound-rtp.bytes_received" => 678239,
          "inbound-rtp.encoding" => :OPUS,
          "inbound-rtp.keyframe_request_sent" => 14,
          "inbound-rtp.packets" => 7264,
          "inbound-rtp.ssrc" => 4131688258
        }
      }
    }
  }
  ```
  """

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    rtc_engine_metrics() ++ Membrane.RTP.Metrics.metrics()
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
