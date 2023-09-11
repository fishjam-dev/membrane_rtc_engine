# Membrane Funnel plugin

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_funnel_plugin.svg)](https://hex.pm/packages/membrane_funnel_plugin)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_funnel_plugin/)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_funnel_plugin.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_funnel_plugin)

Membrane plugin for merging multiple input streams into a single output.

It can be used for collecting data from multiple inputs and sending it through one output.

It is part of [Membrane Multimedia Framework](https://membraneframework.org).

## Installation

The package can be installed by adding `membrane_funnel_plugin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_funnel_plugin, "~> 0.7.0"}
  ]
end
```

## Usage
Playing this pipeline should merge two files `a.txt` and `b.txt` into file `c.txt`

```elixir
defmodule FunnelDemo do
  use Membrane.Pipeline

  def handle_init(_) do
    children = [
      a_source: %Membrane.File.Source{location: "a.txt"},
      b_source: %Membrane.File.Source{location: "b.txt"},
      funnel: Membrane.Funnel,
      sink: %Membrane.File.Sink{location: "c.txt"}
    ]

    links = [
      link(:a_source) |> to(:funnel),
      link(:b_source) |> to(:funnel),
      link(:funnel) |> to(:sink)
    ]

    spec = %ParentSpec{children: children, links: links}
    {{:ok, spec: spec, playback: :playing}, %{}}
  end
end
```

## Copyright and License

Copyright 2020, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_funnel_plugin)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_funnel_plugin)

Licensed under the [Apache License, Version 2.0](LICENSE)
