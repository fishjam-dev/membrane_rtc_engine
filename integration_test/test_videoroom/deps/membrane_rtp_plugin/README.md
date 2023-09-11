# Membrane RTP plugin

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtp_plugin.svg)](https://hex.pm/packages/membrane_rtp_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtp_plugin/)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_rtp_plugin.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_rtp_plugin)

This package provides bins and elements for sending and receiving RTP/SRTP and RTCP/SRTCP streams.

It is a part of [Membrane Multimedia Framework](https://membraneframework.org).

## Installation

The package can be installed by adding `membrane_rtp_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtp_plugin, "~> 0.22.0"},
    {:ex_libsrtp, "~> 0.6.0"} # required only if SRTP/SRTCP support is needed
  ]
end
```

If SRTP/SRTCP support is needed, one has to install `libsrtp` to their system.

### MacOS

Run `brew install srtp`

### Ubuntu

Run `apt install libsrtp2-dev`

### Other

For more details and manual installation, see [ExLibSRTP HexDocs](https://hexdocs.pm/ex_libsrtp/readme.html).

## Usage

For usage examples, check the [RTP demo](https://github.com/membraneframework/membrane_demo/tree/master/rtp).

The docs can be found at [HexDocs](https://hexdocs.pm/membrane_rtp_plugin).

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtp_plugin)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_rtp_plugin)

Licensed under the [Apache License, Version 2.0](LICENSE)
