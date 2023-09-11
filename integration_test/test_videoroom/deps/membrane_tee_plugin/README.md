# Membrane Tee Plugin

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_tee_plugin.svg)](https://hex.pm/packages/membrane_tee_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_tee_plugin)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_tee_plugin.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_tee_plugin)

This package provides elements that can be used to branch stream processing in pipeline, e.g. send data from one source to two or more sinks.

## Installation

This package can be installed by adding `membrane_tee_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_tee_plugin, "~> 0.10.1"}
  ]
end
```

The docs can be found at [HexDocs](https://hexdocs.pm/membrane_tee_plugin).

## Examples

### `Membrane.Tee.Parallel`

This element has dynamic `:output` pads working in `:pull` mode. Packets are forwarded
only when all output pads send demands, which means that the slowest output pad dictates
the speed of processing data.

Playing this pipeline should result in copying the source file to all destination files (sinks).
Before playing it, make sure that the source file exists, e.g. like this:
`echo "Membrane Framework is cool" > /tmp/source_text_file`

You also need [`:membrane_file_plugin`](https://github.com/membraneframework/membrane_file_plugin) in project dependencies to run this pipeline.

```elixir
defmodule FileMultiForwardPipeline do
  use Membrane.Pipeline
  alias Membrane.{File, Tee}

  @impl true
  def handle_init(_ctx, _state) do

    links = [
      child(:tee, Tee.Parallel),
      child(file_src, %File.Source{location: "/tmp/source_text_file"}) 
      |> get_child(:tee) |> child(:file_sink1, %File.Sink{location: "/tmp/destination_file1"}),
      get_child(:tee) |> child(:file_sink2, %File.Sink{location: "/tmp/destination_file2"})
    ]

    {[spec: links], %{}}
  end
end
```

### `Membrane.Tee.PushOutput`

This element works like `Membrane.Tee.Parallel` but doesn't care about the speed of processing of elements connected to its outputs.
If such an element doesn't process data fast enough, it crashes with the toilet overflow error. In other words, the input pad dictates
the speed of processing.

### `Membrane.Tee.Master`

This element has one `:master` output pad that dictates the speed of processing data
and dynamic `:copy` pad working in `:push` mode mirroring `:master` pad.

Playing this pipeline should result in playing mp3 source file and copying it to the destination file.
Before playing it, make sure you have valid source file, e.g. [this one](https://github.com/membraneframework/membrane-demo/blob/v0.3/sample.mp3).

You also need [`:membrane_file_plugin`](https://github.com/membraneframework/membrane_file_plugin) and [`:membrane_portaudio_plugin`](https://github.com/membraneframework/membrane_portaudio_plugin) in project dependencies to run this pipeline.

```elixir
defmodule AudioPlayAndCopyPipeline do
  use Membrane.Pipeline
  alias Membrane.{File, Tee, PortAudio}

  @impl true
  def handle_init(_ctx, _opts) do
    links = [
      child(:tee, Tee.Master),
      child(:file_src, %File.Source{location: "/tmp/source_file.mp3"}) |> get_child(:tee),
      get_child(:tee) |> via_out(:master) |> child(:audio_sink,PortAudio.Sink),
      get_child(:tee) |> via_out(:copy) |> to(:file_sink,%File.Sink{location: "/tmp/destination_file.mp3"})
    ]

    {[spec: links], %{}}
  end
end
```

## Copyright and License

Copyright 2019, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_tee_plugin)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_tee_plugin)

Licensed under the [Apache License, Version 2.0](LICENSE)
