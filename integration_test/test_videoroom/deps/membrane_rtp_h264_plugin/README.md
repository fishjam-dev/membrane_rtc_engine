# Membrane RTP H264 plugin
[![Hex.pm](https://img.shields.io/hexpm/v/membrane_rtp_h264_plugin.svg)](https://hex.pm/packages/membrane_rtp_h264_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_rtp_h264_plugin/)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_rtp_h264_plugin.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_rtp_h264_plugin)

RTP payloader and depayloader for H264.

It is part of [Membrane Multimedia Framework](https://membraneframework.org).

## Usage

This plugin registers default payloader and depayloader for H264 [RTP payload format](https://hexdocs.pm/membrane_rtp_format/Membrane.RTP.PayloadFormat.html) and thus can be automatically used by [Membrane RTP plugin](https://hexdocs.pm/membrane_rtp_plugin) whenever added to dependencies. Of course it can be manually linked in a custom pipeline too.

## Supported packetization modes

This package currently supports only
Single Nal Unit Mode and Non-Interleaved (STAP-A and FU-A) packetization modes.
Interleaved mode is currently not supported. Please refer to [RFC 6184](https://tools.ietf.org/html/rfc6184) for details.

## Installation

The package can be installed by adding `membrane_rtp_h264_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_rtp_h264_plugin, "~> 0.15.1"}
  ]
end
```

The docs can be found at [HexDocs](https://hexdocs.pm/membrane_rtp_h264_plugin).

## Copyright and License

Copyright 2019, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

Licensed under the [Apache License, Version 2.0](LICENSE)
