# Endpoints

Directory containing different Endpoints of the `membrane_rtc_engine`.

Each Endpoint is a separate package with its own source files, dependencies and tests.
Ultimately, at a certain point in the future, the Endpoints are going to be entirely
separate from each other (right now, due to certain shared modules, the `HLS` and `RTSP` Endpoints
both depend on the `WebRTC` Endpoint).

To use a certain Endpoint in your app, you have to declare it in your dependencies list (as well as
the Engine), e.g.
```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.16.0"},
    {:membrane_rtc_engine_endpoint_webrtc, "~> 0.1.0"}
  ]
end
```

The directory `integration_test/` contains test scenarios utilising multiple Endpoints of different
types.

## Testing

To test an Endpoint, navigate to its directory and run `mix test`.

Alternatively, you can use convenience aliases from the root of the Engine repo:
`mix test.webrtc`, `mix test.hls`, `mix test.rtsp` and `mix test.integration`.

Or, to run all four of the above commands as well as the engine tests, use the alias `mix test.all`
from the root of the Engine repo.
