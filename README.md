# Membrane SFU

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_sfu_.svg)](https://hex.pm/packages/membrane_sfu)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_sfu)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_sfu.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_sfu)

Client and server libraries for Membrane SFU.

## Documentation

Documentation of client library is available at https://hexdocs.pm/membrane_sfu/js

Documentation of server library is available at https://hexdocs.pm/membrane_sfu

## Installation

The package can be installed by adding `membrane_sfu` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_sfu, "~> 0.1.0"}
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

Copyright 2021, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_sfu)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_sfu)

Licensed under the [Apache License, Version 2.0](LICENSE)
