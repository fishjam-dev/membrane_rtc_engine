# Membrane ICE plugin

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_ice_plugin.svg)](https://hex.pm/packages/membrane_ice_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_libnice_plugin/)
[![CircleCI](https://circleci.com/gh/jellyfish-dev/membrane_ice_plugin.svg?style=svg)](https://circleci.com/gh/jellyfish-dev/membrane_ice_plugin)

Membrane plugin for ICE protocol.

It enables establishing connection, sending and receiving messages using ICE protocol.

This package uses [fake_turn] and is part of [Membrane Multimedia Framework](https://membrane.stream).

## Installation

The package can be installed by adding `membrane_ice_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_ice_plugin, "~> 0.15.1"}
  ]
end
```

### Additional dependencies

#### MacOS Intel

```
brew install openssl
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"
```

To run tests:

```
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CFLAGS="-I/usr/local/opt/openssl@1.1/include/"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include/"
```

## Usage

See `Membrane.WebRTC.EndpointBin` in [membrane_webrtc_plugin](https://hex.pm/packages/membrane_webrtc_plugin) for usage example

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_ice)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_ice)

Licensed under the [Apache License, Version 2.0](LICENSE)
