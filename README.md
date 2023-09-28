# Membrane RTC Engine

Customizable Real-time Communication Engine/SFU library focused on WebRTC.

## Usage

For usage examples, please refer to our [membrane_demo](https://github.com/membraneframework/membrane_demo/tree/master/webrtc_videoroom) or
[membrane_videoroom](https://github.com/membraneframework/membrane_videoroom) repositories.

## Repository structure

This repository currently holds the following packages:

- [`engine`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/engine) - RTC Engine,
  the main package responsible for exchanging media tracks between Endpoints,
- [`webrtc`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/webrtc) -
  WebRTC Endpoint, responsible for establishing a connection with some WebRTC client (mainly browser) and exchanging media with it,
- [`hls`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/hls) -
  HLS Endpoint, responsible for receiving media tracks from all other Endpoints and saving them to files by creating HLS playlists,
- [`rtsp`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/rtsp) -
  RTSP Endpoint, responsible for connecting to a remote RTSP stream source and sending the appropriate media track to other Endpoints.

For more info about a given Endpoint, refer to its documentation.

Each Endpoint is a separate package with its own source files, dependencies and tests.
Ultimately, at a certain point in the future, the Endpoints are going to be entirely
separate from each other (right now, due to certain shared modules, the `HLS` and `RTSP` Endpoints
both depend on the `WebRTC` Endpoint).

To use a certain Endpoint in your app, you have to declare it in your dependencies list (as well as
the Engine), e.g.
```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.17.1"},
    {:membrane_rtc_engine_webrtc, "~> 0.3.0"}
  ]
end
```

The [`integration_test/`](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/integration_test) directory
contains test scenarios utilising multiple Endpoints of different types.
