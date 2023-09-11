# Unifex

[![Hex.pm](https://img.shields.io/hexpm/v/unifex.svg)](https://hex.pm/packages/unifex)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/unifex/)
[![CircleCI](https://circleci.com/gh/membraneframework/unifex.svg?style=svg)](https://circleci.com/gh/membraneframework/unifex)

Unifex is a tool for generating interfaces between native C code and Elixir, that:
- provides intuitive and concise tools for defining native interfaces,
- generates all the boilerplate for you,
- provides useful abstractions over binaries and state,
- makes native code independent from [erl_nif](https://erlang.org/doc/man/erl_nif.html) 
  or [ei](https://erlang.org/doc/man/ei.html) library, so the same code is usable either with NIFs or CNodes.

API documentation is available at [HexDocs](https://hexdocs.pm/unifex/).

Unifex uses [Bundlex](https://github.com/membraneframework/bundlex) to compile and load the native code. See there for supported platforms & details.

This tool is maintained by the [Membrane Framework](https://membraneframework.org/) team.

## Installation

To install, you need to configure Mix project as follows:

```elixir
defmodule MyApp.Mixfile do
  use Mix.Project

  def project do
    [
      app: :my_app,
      compilers: [:unifex, :bundlex] ++ Mix.compilers, # add unifex and bundlex to compilers
      ...,
      deps: deps()
   ]
  end

  defp deps() do
    [
      {:unifex, "~> 1.1"}
    ]
  end
end
```

## Usage

For detailed usage description see [Creating Unifex Natives](https://hexdocs.pm/unifex/creating_unifex_natives.html) guide.

## Supported types

For currently supported types see [Supported Types](https://hexdocs.pm/unifex/supported_types.html) section.

## See also

Unifex depends on the following libraries:
- [Bundlex](https://github.com/membraneframework/bundlex)
- [Shmex](https://github.com/membraneframework/shmex)

## Copyright and License

Copyright 2018, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

Licensed under the [Apache License, Version 2.0](LICENSE)
