# Membrane RTC Engine

Customizable Real-time Communication Engine/SFU library focused on WebRTC.

## Usage

For usage examples, please refer to:

- [the `examples/` directory of this repository](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/examples/),
- our [membrane\_videoroom](https://github.com/membraneframework/membrane_videoroom) repository,
- our [jellyfish](https://github.com/jellyfish-dev/jellyfish) repository.

## Repository structure

This repository currently holds the following packages:

- [`engine`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/engine) -
  RTC Engine, the main package responsible for exchanging media tracks between Endpoints,
- [`webrtc`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/webrtc) -
  WebRTC Endpoint, responsible for establishing a connection with some WebRTC client (mainly browser) and exchanging media with it,
- [`hls`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/hls) -
  HLS Endpoint, responsible for receiving media tracks from all other Endpoints and saving them to files by creating HLS playlists,
- [`rtsp`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/rtsp) -
  RTSP Endpoint, responsible for connecting to a remote RTSP stream source and sending the appropriate media track to other Endpoints,
- [`file`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/file) -
  File Endpoint, responsible for reading track from a file, payloading it into RTP, and sending it to other Endpoints.

For more info about a given Endpoint, refer to its documentation.

Each Endpoint is a separate package with its own source files, dependencies and tests.
To use a certain Endpoint in your app, you have to declare it in your dependencies list (as well as
the Engine), e.g.
```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.19.0"},
    {:membrane_rtc_engine_webrtc, "~> 0.5.0"}
  ]
end
```

[The `integration_test/` directory](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/integration_test)
contains test scenarios utilising multiple Endpoints of different types.
