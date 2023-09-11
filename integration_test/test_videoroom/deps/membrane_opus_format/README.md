# Membrane OPUS Format

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_opus_format.svg)](https://hex.pm/packages/membrane_opus_format)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_opus_format/)

This package provides Opus audio format description for the
[Membrane Multimedia Framework](https://membraneframework.org).

# Installation

Unless you're developing a Membrane Element it's unlikely you need to
use this package directly in your app, as normally it is going to be fetched as
a dependency of any element that operates on Opus encoded audio.

However, if you are developing an Element or need to add it due to any other
reason, the package can be installed by adding `membrane_opus_format` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_opus_format, "~> 0.3.0"}
  ]
end
```

## Copyright and License

Copyright 2019, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_opus_format)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_opus_format)

Licensed under the [Apache License, Version 2.0](LICENSE)
