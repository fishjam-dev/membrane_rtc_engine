# Metrics

RTC Engine uses [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics) to aggregate data about media streams and generate reports about them.
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
  {:room_id, "test"} => %{
    {:peer_id, "7eda6931-0313-497e-93a0-6a9540407f77"} => %{
      :"ice.binding_requests_received" => 3,
      :"ice.binding_responses_sent" => 3,
      :"ice.bytes_received" => 20672,
      :"ice.bytes_sent" => 1634,
      :"ice.packets_received" => 46,
      :"ice.packets_sent" => 3,
      :"ice.port" => 51895,
      :"ice.protocol" => :udp,
      :"peer.metadata" => nil,
      :"sdp.answer" => "v=0\r\no=- ...", 
      :"sdp.offer" => "v=0\r\no=- ...", 
      {:track_id,
       "7eda6931-0313-497e-93a0-6a9540407f77:3d228c10-d3b9-4009-b14f-4b0f2b89f7ba:l"} => %{
        "inbound-rtp.bytes_received": 6470,
        "inbound-rtp.encoding": :H264,
        "inbound-rtp.markers_received": 4,
        "inbound-rtp.packets": 10,
        "inbound-rtp.ssrc": 1546136873,
        rtx_stream: %{
          "inbound-rtp.bytes_received": 5120,
          "inbound-rtp.encoding": :rtx,
          "inbound-rtp.markers_received": 1,
          "inbound-rtp.packets": 6,
          "inbound-rtp.ssrc": 84792660
        },
        "track.metadata": %{"active" => true, "type" => "camera"}
      },
      {:track_id,
       "7eda6931-0313-497e-93a0-6a9540407f77:3d228c10-d3b9-4009-b14f-4b0f2b89f7ba:m"} => %{
        "inbound-rtp.bytes_received": 5988,
        "inbound-rtp.encoding": :H264,
        "inbound-rtp.packets": 6,
        "inbound-rtp.ssrc": 3428415963,
        "track.metadata": %{"active" => true, "type" => "camera"}
      },
      {:track_id,
       "7eda6931-0313-497e-93a0-6a9540407f77:90ce43b1-d37a-452e-8a04-b2883e7d54dc:"} => %{
        "inbound-rtp.bytes_received": 1885,
        "inbound-rtp.encoding": :OPUS,
        "inbound-rtp.markers_received": 1,
        "inbound-rtp.packets": 18,
        "inbound-rtp.ssrc": 3178961132,
        "rtcp.total_packets_sent": 1,
        "track.metadata": %{"active" => true, "type" => "audio"}
      }
    }
  }
}
```

You can also go to [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom) or [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics) docs for more examples.
