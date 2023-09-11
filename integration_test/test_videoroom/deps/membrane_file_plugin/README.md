# Membrane File plugin

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_file_plugin.svg)](https://hex.pm/packages/membrane_file_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_file_plugin/)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_file_plugin.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_file_plugin)

Membrane plugin for reading and writing to files.

It is part of [Membrane Multimedia Framework](https://membraneframework.org).

## Installation

The package can be installed by adding `membrane_file_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_file_plugin, "~> 0.13.2"}
  ]
end
```

## Usage examples

### File.Sink and File.Source

`Source` and `Sink` elements allow reading from and writing to a file, respectively.
The pipeline in `./examples/sink_and_source.exs` will copy the contents of that script to `/tmp/example.exs`

### File.MultiSink

`MultiSink` allows writing to multiple files, with the input being split into parts.
The example in `./examples/sink_multi.exs` will generate 0-filled input file of 1024 bytes (`input.bin`)
and copy first 10-bytes to `/tmp/output0.bin` and the rest to `/tmp/output1.bin`.

## Copyright and License

Copyright 2018, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

Licensed under the [Apache License, Version 2.0](LICENSE)
