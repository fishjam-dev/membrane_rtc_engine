# Metrics

RTC Engine uses [`membrane_telemetry_metrics`](github.com/membraneframework/membrane_telemetry_metrics) to aggregate data about media streams and generate reports about them.
To enable metrics aggregation, you have to put line 
```elixir
config :membrane_telemetry_metrics, enabled: true
```
in your config file and start `Membrane.TelemetryMetrics.Reporter` with RTC Engine metrics by calling
```elixir 
{:ok, reporter} = Membrane.TelemetryMetrics.Reporter.start_link(Membrane.RTC.Engine.Metrics.metrics())
```

Then, if you want to get a report with metrics values for every running RTC Engine on the node, you have to call
```elixir
Membrane.TelemetryMetrics.Reporter.scrape(reporter)
```

There is a report example below, with only one room with one peer inside
```elixir
%{
  {:room_id, "my_meeting"} => %{
    {:peer_id, "aa83837e-c18a-4d0e-b7f1-565acdddd3e0"} => %{
      {:track_id,
       "aa83837e-c18a-4d0e-b7f1-565acdddd3e0:2cfee338-37f9-46cf-8c5b-d3909caa4dce:l"} => %{
        "inbound-rtp.VP8.frames" => 5450,
        "inbound-rtp.VP8.keyframes" => 46,
        "inbound-rtp.bytes_received" => 4106974,
        "inbound-rtp.encoding" => :VP8,
        "inbound-rtp.keyframe_request_sent" => 22,
        "inbound-rtp.packets" => 6099,
        "inbound-rtp.ssrc" => 1507540019
      },
      {:track_id,
       "aa83837e-c18a-4d0e-b7f1-565acdddd3e0:2cfee338-37f9-46cf-8c5b-d3909caa4dce:m"} => %{
        "inbound-rtp.VP8.frames" => 5303,
        "inbound-rtp.VP8.keyframes" => 45,
        "inbound-rtp.bytes_received" => 21999552,
        "inbound-rtp.encoding" => :VP8,
        "inbound-rtp.keyframe_request_sent" => 22,
        "inbound-rtp.packets" => 20757,
        "inbound-rtp.ssrc" => 1441607303
      },
      {:track_id,
       "aa83837e-c18a-4d0e-b7f1-565acdddd3e0:a8ffd71d-932a-4955-b4da-d941449aa329:"} => %{
        "inbound-rtp.OPUS.frames" => 11348,
        "inbound-rtp.bytes_received" => 1175977,
        "inbound-rtp.encoding" => :OPUS,
        "inbound-rtp.keyframe_request_sent" => 22,
        "inbound-rtp.packets" => 11348,
        "inbound-rtp.ssrc" => 3192426135
      },
      "ice.binding_requests_received" => 94,
      "ice.binding_responses_sent" => 94,
      "ice.bytes_received" => 27329251,
      "ice.bytes_sent" => 76030,
      "ice.packets_received" => 39189,
      "ice.packets_sent" => 975
    }
  }
}
```

You can also go to ['membrane_videoroom`](github.com/membraneframework/membrane_videoroom) or [`membrane_telemetry_metrics`](github.com/membraneframework/membrane_telemetry_metrics) docs for more examples.