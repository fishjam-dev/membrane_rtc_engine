# BiMap

[![Build Status](https://github.com/mkaput/elixir-bimap/workflows/CI/badge.svg)](https://github.com/mkaput/elixir-bimap/actions?query=workflow%3ACI)
[![Version](https://img.shields.io/hexpm/v/bimap.svg)](https://hex.pm/packages/bimap)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/bimap/)
[![Download](https://img.shields.io/hexpm/dt/bimap.svg)](https://hex.pm/packages/bimap)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Last Updated](https://img.shields.io/github/last-commit/mkaput/elixir-bimap.svg)](https://github.com/mkaput/elixir-bimap/commits/master)

Elixir implementation of bidirectional map (`BiMap`) and multimap (`BiMultiMap`).

## Installation

The package can be installed by adding `bimap` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:bimap, "~> 1.3"}]
end
```

## Getting started

For more examples, checkout [`BiMap`](https://hexdocs.pm/bimap/BiMap.html) and [`BiMultiMap`](https://hexdocs.pm/bimap/BiMultiMap.html) on hex docs.

### BiMap

```elixir
iex(1)> Mix.install [:bimap]
iex(2)> bm = BiMap.new(a: 1, b: 2)
BiMap.new([a: 1, b: 2])
iex(3)> BiMap.get(bm, :a)
1
iex(4)> BiMap.get_key(bm, 2)
:b
iex(5)> BiMap.put(bm, :a, 3)
BiMap.new([a: 3, b: 2])
iex(6)> BiMap.put(bm, :c, 2)
BiMap.new([a: 1, c: 2])
```

### BiMultiMap

```elixir
iex(1)> Mix.install [:bimap]
iex(2)> mm = BiMultiMap.new(a: 1, b: 2, b: 1)
BiMultiMap.new([a: 1, b: 1, b: 2])
iex(3)> BiMultiMap.get(mm, :a)
[1]
iex(4)> BiMultiMap.get_keys(mm, 1)
[:a, :b]
iex(5)> BiMultiMap.put(mm, :a, 3)
BiMultiMap.new([a: 1, a: 3, b: 1, b: 2])
```

## Changelog

All notable changes to this project are documented on the [GitHub releases] page.

## License

See the [LICENSE] file for license rights and limitations (MIT).

[github releases]: https://github.com/mkaput/elixir-bimap/releases
[license]: https://github.com/mkaput/elixir-bimap/blob/master/LICENSE.txt
