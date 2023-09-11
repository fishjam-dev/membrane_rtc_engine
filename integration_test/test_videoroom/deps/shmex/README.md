# Shmex

[![Hex.pm](https://img.shields.io/hexpm/v/shmex.svg)](https://hex.pm/packages/shmex)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/shmex/)
[![CircleCI](https://circleci.com/gh/membraneframework/shmex.svg?style=svg)](https://circleci.com/gh/membraneframework/shmex)

Shmex is a library providing Elixir bindings for shared memory and native
functions to manipulate it in NIFs.

Documentation is available at [HexDocs](https://hexdocs.pm/shmex)

The source code is available at [GitHub](https://github.com/membraneframework/shmex)

## Installation

Add the following line to your `deps` in `mix.exs`. Run `mix deps.get`.

```elixir
	{:shmex, "~> 0.5.0"}
```

All native stuff is exported in `:lib` and `:shmex` [Bundlex](https://hex.pm/packages/bundlex) dependencies.
To import, add the following line to your nif specification in `bundlex.exs`
```elixir
deps: [shmex: :lib]
```
and another one in your native header file
```c
#import <shmex/lib.h>
```

## Testing

To execute tests run `mix test`. These test tags are excluded by default:
- `shm_tmpfs` - tests that require access to information about shared memory segments present in the OS via tmpfs, not supported e.g. by Mac OS
- `shm_resizable` - tests for functions that involve resizing existing shared memory segments, not supported e.g. by Mac OS

## Copyright and License

Copyright 2018, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane)

Licensed under the [Apache License, Version 2.0](LICENSE)
