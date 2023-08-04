# Membrane RTC Engine

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtc_engine.svg)](https://hex.pm/packages/membrane_rtc_engine)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtc_engine)
[![codecov](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine/branch/master/graph/badge.svg?token=9F1XHHUY2B)](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine)
[![CircleCI](https://circleci.com/gh/jellyfish-dev/membrane_rtc_engine.svg?style=svg)](https://circleci.com/gh/jellyfish-dev/membrane_rtc_engine)

Customizable Real-time Communication Engine/SFU library focused on WebRTC.

## Installation

The package can be installed by adding `membrane_rtc_engine` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.16.0"}
  ]
end
```

To use a given Endpoint, you have to include it in your list of dependencies as well:
```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.16.0"},
    {:membrane_rtc_engine_webrtc, "~> 0.1.0"}
  ]
end
```

## Usage

For usage examples, please refer to our [membrane_demo](https://github.com/membraneframework/membrane_demo/tree/master/webrtc_videoroom) or
[membrane_videoroom](https://github.com/membraneframework/membrane_videoroom) repositories.

## Developing

To make development a little easier, we have added several tasks:
- `mix test.all`, which runs unit tests from the engine, unit tests from each endpoint, and
  endpoint integration tests,
- `mix test.webrtc.integration`, which runs WebRTC Endpoint integration tests
  (present [here](https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/webrtc/integration_test/test_videoroom)).
- To test a given Endpoint, you can use the following aliases: `mix test.webrtc`,
  `mix test.hls`, `mix test.rtsp` and `mix test.integration`.

## Copyright and License

Copyright 2021, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

Licensed under the [Apache License, Version 2.0](LICENSE)
