# Coerce

[![hex.pm version](https://img.shields.io/hexpm/v/coerce.svg)](https://hex.pm/packages/coerce)
[![Build Status](https://travis-ci.org/Qqwy/elixir-coercei.svg?branch=master)](https://travis-ci.org/Qqwy/elixir-coerce)

  Coerce allows defining coercions between data types.

  These are standardized conversions of one kind of data to another.
  A coercion can be defined using `defcoercion.

  The code that coercion is compiled to attempts to ensure that the result
  is relatively fast (with the possibility for further optimization in the future).
  

  Coerce does _not_ come with built-in coercions, instead allowing libraries that build on top of it
  to define their own rules.


## Examples

```elixir

      iex> require Coerce
      iex> Coerce.defcoercion(Integer, Float) do
      iex>   def coerce(int, float) do
      iex>     {int + 0.0, float}
      iex>   end
      iex> end
      iex> Coerce.coerce(1, 2.3)
      {1.0, 2.3}
      iex> Coerce.coerce(1.4, 42)
      {1.4, 42.0}

```

```elixir

      iex> require Coerce
      iex> Coerce.defcoercion(BitString, Atom) do
      iex>   def coerce(str, atom) do
      iex>     {str, inspect(atom)}
      iex>   end
      iex> end
      iex> Coerce.coerce("foo", Bar)
      {"foo", "Bar"}
      iex> Coerce.coerce("baz", :qux)
      {"baz", ":qux"}
```

## Installation

The package can be installed
by adding `coerce` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:coerce, "~> 1.0.0"}
  ]
end
```

Documentation can
be found at [https://hexdocs.pm/coerce](https://hexdocs.pm/coerce).

## Changelog

- 1.0.1 - Coercion implementation modules no longer show up in generated documentation.
- 1.0.0 - First feature-complete stable release
