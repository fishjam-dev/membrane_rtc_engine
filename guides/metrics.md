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
      :"sdp.answer" => "v=0\r\no=- 47437144050972676 0 IN IP4 127.0.0.1\r\ns=-\r\nt=0 0\r\na=group:BUNDLE 0 1\r\na=extmap-allow-mixed\r\na=ice-lite\r\nm=video 9 UDP/TLS/RTP/SAVPF 106 107\r\nc=IN IP4 0.0.0.0\r\na=recvonly\r\na=ice-ufrag:****\na=ice-pwd:****\na=ice-options:trickle\r\na=fingerprint:****\na=setup:passive\r\na=mid:0\r\na=msid:dba76f7e-0a63-4a94-966d-c32247f61bad 4e3abb36-7aec-48dc-aa5d-3385b285acf5\r\na=rtcp-mux\r\na=rtpmap:106 H264/90000\r\na=fmtp:106 profile-level-id=42e01f;level-asymmetry-allowed=1;packetization-mode=1\r\na=rtpmap:107 rtx/90000\r\na=fmtp:107 apt=106\r\na=extmap:4 http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01\r\na=rtcp-fb:106 transport-cc\r\na=extmap:9 urn:ietf:params:rtp-hdrext:sdes:mid\r\na=extmap:10 urn:ietf:params:rtp-hdrext:sdes:rtp-stream-id\r\na=extmap:11 urn:ietf:params:rtp-hdrext:sdes:repaired-rtp-stream-id\r\na=rtcp-fb:106 ccm fir\r\na=rtcp-fb:106 nack\r\na=rtcp-fb:106 nack pli\r\na=rtcp-rsize\r\na=rid:l recv\r\na=rid:m recv\r\na=rid:h recv\r\na=simulcast:recv l;m;h\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111\r\nc=IN IP4 0.0.0.0\r\na=recvonly\r\na=ice-ufrag:****\na=ice-pwd:****\na=ice-options:trickle\r\na=fingerprint:****\na=setup:passive\r\na=mid:1\r\na=msid:dba76f7e-0a63-4a94-966d-c32247f61bad 9110ddda-8f17-46c5-ac2d-0496a335d6de\r\na=rtcp-mux\r\na=rtpmap:111 opus/48000/2\r\na=fmtp:111 minptime=10;useinbandfec=1\r\na=extmap:14 urn:ietf:params:rtp-hdrext:ssrc-audio-level vad=on\r\na=extmap:4 http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01\r\na=rtcp-fb:111 transport-cc\r\na=extmap:9 urn:ietf:params:rtp-hdrext:sdes:mid\r\n",
      :"sdp.offer" => "v=0\r\no=- 5060319063525960072 2 IN IP4 127.0.0.1\r\ns=-\r\nt=0 0\r\na=group:BUNDLE 0 1\r\na=extmap-allow-mixed\r\na=msid-semantic: WMS\r\nm=video 9 UDP/TLS/RTP/SAVPF 96 97 102 103 104 105 106 107 108 109 127 125 39 40 45 46 98 99 100 101 112 113 114\r\nc=IN IP4 0.0.0.0\r\na=rtcp:9 IN IP4 0.0.0.0\r\na=ice-ufrag:****\na=ice-pwd:****\na=ice-options:trickle\r\na=fingerprint:****\na=setup:actpass\r\na=mid:0\r\na=extmap:1 urn:ietf:params:rtp-hdrext:toffset\r\na=extmap:2 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time\r\na=extmap:3 urn:3gpp:video-orientation\r\na=extmap:4 http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01\r\na=extmap:5 http://www.webrtc.org/experiments/rtp-hdrext/playout-delay\r\na=extmap:6 http://www.webrtc.org/experiments/rtp-hdrext/video-content-type\r\na=extmap:7 http://www.webrtc.org/experiments/rtp-hdrext/video-timing\r\na=extmap:8 http://www.webrtc.org/experiments/rtp-hdrext/color-space\r\na=extmap:9 urn:ietf:params:rtp-hdrext:sdes:mid\r\na=extmap:10 urn:ietf:params:rtp-hdrext:sdes:rtp-stream-id\r\na=extmap:11 urn:ietf:params:rtp-hdrext:sdes:repaired-rtp-stream-id\r\na=sendonly\r\na=msid:- 6f8345bc-4b9b-466c-960c-4a95ee7e5615\r\na=rtcp-mux\r\na=rtcp-rsize\r\na=rtpmap:96 VP8/90000\r\na=rtcp-fb:96 goog-remb\r\na=rtcp-fb:96 transport-cc\r\na=rtcp-fb:96 ccm fir\r\na=rtcp-fb:96 nack\r\na=rtcp-fb:96 nack pli\r\na=rtpmap:97 rtx/90000\r\na=fmtp:97 apt=96\r\na=rtpmap:102 H264/90000\r\na=rtcp-fb:102 goog-remb\r\na=rtcp-fb:102 transport-cc\r\na=rtcp-fb:102 ccm fir\r\na=rtcp-fb:102 nack\r\na=rtcp-fb:102 nack pli\r\na=fmtp:102 level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f\r\na=rtpmap:103 rtx/90000\r\na=fmtp:103 apt=102\r\na=rtpmap:104 H264/90000\r\na=rtcp-fb:104 goog-remb\r\na=rtcp-fb:104 transport-cc\r\na=rtcp-fb:104 ccm fir\r\na=rtcp-fb:104 nack\r\na=rtcp-fb:104 nack pli\r\na=fmtp:104 level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42001f\r\na=rtpmap:105 rtx/90000\r\na=fmtp:105 apt=104\r\na=rtpmap:106 H264/90000\r\na=rtcp-fb:106 goog-remb\r\na=rtcp-fb:106 transport-cc\r\na=rtcp-fb:106 ccm fir\r\na=rtcp-fb:106 nack\r\na=rtcp-fb:106 nack pli\r\na=fmtp:106 level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f\r\na=rtpmap:107 rtx/90000\r\na=fmtp:107 apt=106\r\na=rtpmap:108 H264/90000\r\na=rtcp-fb:108 goog-remb\r\na=rtcp-fb:108 transport-cc\r\na=rtcp-fb:108 ccm fir\r\na=rtcp-fb:108 nack\r\na=rtcp-fb:108 nack pli\r\na=fmtp:108 level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42e01f\r\na=rtpmap:109 rtx/90000\r\na=fmtp:109 apt=108\r\na=rtpmap:127 H264/90000\r\na=rtcp-fb:127 goog-remb\r\na=rtcp-fb:127 transport-cc\r\na=rtcp-fb:127 ccm fir\r\na=rtcp-fb:127 nack\r\na=rtcp-fb:127 nack pli\r\na=fmtp:127 level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f\r\na=rtpmap:125 rtx/90000\r\na=fmtp:125 apt=127\r\na=rtpmap:39 H264/90000\r\na=rtcp-fb:39 goog-remb\r\na=rtcp-fb:39 transport-cc\r\na=rtcp-fb:39 ccm fir\r\na=rtcp-fb:39 nack\r\na=rtcp-fb:39 nack pli\r\na=fmtp:39 level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=4d001f\r\na=rtpmap:40 rtx/90000\r\na=fmtp:40 apt=39\r\na=rtpmap:45 AV1/90000\r\na=rtcp-fb:45 goog-remb\r\na=rtcp-fb:45 transport-cc\r\na=rtcp-fb:45 ccm fir\r\na=rtcp-fb:45 nack\r\na=rtcp-fb:45 nack pli\r\na=rtpmap:46 rtx/90000\r\na=fmtp:46 apt=45\r\na=rtpmap:98 VP9/90000\r\na=rtcp-fb:98 goog-remb\r\na=rtcp-fb:98 transport-cc\r\na=rtcp-fb:98 ccm fir\r\na=rtcp-fb:98 nack\r\na=rtcp-fb:98 nack pli\r\na=fmtp:98 profile-id=0\r\na=rtpmap:99 rtx/90000\r\na=fmtp:99 apt=98\r\na=rtpmap:100 VP9/90000\r\na=rtcp-fb:100 goog-remb\r\na=rtcp-fb:100 transport-cc\r\na=rtcp-fb:100 ccm fir\r\na=rtcp-fb:100 nack\r\na=rtcp-fb:100 nack pli\r\na=fmtp:100 profile-id=2\r\na=rtpmap:101 rtx/90000\r\na=fmtp:101 apt=100\r\na=rtpmap:112 red/90000\r\na=rtpmap:113 rtx/90000\r\na=fmtp:113 apt=112\r\na=rtpmap:114 ulpfec/90000\r\na=rid:l send\r\na=rid:m send\r\na=rid:h send\r\na=simulcast:send l;m;h\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111 63 9 0 8 13 110 126\r\nc=IN IP4 0.0.0.0\r\na=rtcp:9 IN IP4 0.0.0.0\r\na=ice-ufrag:****\na=ice-pwd:****\na=ice-options:trickle\r\na=fingerprint:****\na=setup:actpass\r\na=mid:1\r\na=extmap:14 urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\na=extmap:2 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time\r\na=extmap:4 http://www.ietf.org/id/draft-holme" <> ...,
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