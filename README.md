# Membrane RTC Engine

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtc_engine.svg)](https://hex.pm/packages/membrane_rtc_engine)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtc_engine)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_rtc_engine.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_rtc_engine)

Client and server libraries for Membrane RTC Engine.

## Documentation

Documentation of client library is available at https://hexdocs.pm/membrane_rtc_engine/js

Documentation of server library is available at https://hexdocs.pm/membrane_rtc_engine

## Installation

The package can be installed by adding `membrane_rtc_engine` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtc_engine, "~> 0.1.0"}
  ]
end
```

## Usage

For usage examples please refere to our [membrane_demo](https://github.com/membraneframework/membrane_demo/tree/master/webrtc/videoroom) repository.


## Developing

To make the development a little easier we augmented `mix compile` and `mix docs` tasks so that `mix compile` also installs npm dependencies and compiles TypeScript code
and `mix docs` also generates documenation for TypeScript code.

Thanks to this there is no need to include compiled JS code in `priv/static`. It will be generated each time `mix compile` is called.

TypeScript documentation will be generated under `doc/js/`.

## Copyright and License

Copyright 2021, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtc_engine)

Licensed under the [Apache License, Version 2.0](LICENSE)
