# Membrane RTC Engine

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtc_engine.svg)](https://hex.pm/packages/membrane_rtc_engine)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtc_engine)
[![codecov](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine/branch/master/graph/badge.svg?token=9F1XHHUY2B)](https://codecov.io/gh/jellyfish-dev/membrane_rtc_engine)
[![CircleCI](https://circleci.com/gh/jellyfish-dev/membrane_rtc_engine.svg?style=svg)](https://circleci.com/gh/jellyfish-dev/membrane_rtc_engine)

Customizable Real-time Communication Engine/SFU library focused on WebRTC.

## Client SDKs

* [JS/TS](https://github.com/jellyfish-dev/membrane-webrtc-js)
* [Android](https://github.com/jellyfish-dev/membrane-webrtc-android)
* [iOS](https://github.com/jellyfish-dev/membrane-webrtc-ios)
* [React Native](https://github.com/jellyfish-dev/react-native-membrane-webrtc)

## Installation

The package can be installed by adding `membrane_rtc_engine` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.10.2"}
  ]
end
```

## Usage

For usage examples, please refer to our [membrane_demo](https://github.com/membraneframework/membrane_demo/tree/master/webrtc_videoroom) or
[membrane_videoroom](https://github.com/membraneframework/membrane_videoroom) repositories.


## Developing

To make the development a little easier, we have added `mix integration_test` task, which runs integration tests from `integration/test_videoroom`.

## Copyright and License

Copyright 2021, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

Licensed under the [Apache License, Version 2.0](LICENSE)
