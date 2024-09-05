# Membrane RTC Engine WebRTC Endpoint

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtc_engine_webrtc.svg)](https://hex.pm/packages/membrane_rtc_engine_webrtc)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtc_engine_webrtc)
[![codecov](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine/branch/master/graph/badge.svg?token=9F1XHHUY2B)](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine)
[![CircleCI](https://circleci.com/gh/fishjam-dev/membrane_rtc_engine.svg?style=svg)](https://circleci.com/gh/fishjam-dev/membrane_rtc_engine)

WebRTC Endpoint for the [Membrane RTC Engine](https://github.com/jellyfish-dev/membrane_rtc_engine)

## Client SDKs

- [JS/TS](https://github.com/jellyfish-dev/membrane-webrtc-js)
- [Android](https://github.com/jellyfish-dev/membrane-webrtc-android)
- [iOS](https://github.com/jellyfish-dev/membrane-webrtc-ios)
- [React Native](https://github.com/jellyfish-dev/react-native-membrane-webrtc)

## Installation

The package can be installed by adding `membrane_rtc_engine_webrtc` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtc_engine_webrtc, "~> 0.8.0"}
  ]
end
```

## Developing

Integration tests using the `test_videoroom` app can be run by executing `mix test.webrtc.integration`
in the `membrane_rtc_engine` package.

Alternatively, you can use the commands specified [here](integration_test/test_videoroom/README.md).

## Copyright and License

Copyright 2023, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

Licensed under the [Apache License, Version 2.0](LICENSE)
