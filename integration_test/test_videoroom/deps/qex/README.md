# Qex

[![Elixir CI](https://github.com/princemaple/elixir-queue/actions/workflows/elixir.yml/badge.svg)](https://github.com/princemaple/elixir-queue/actions/workflows/elixir.yml)
[![Module Version](https://img.shields.io/hexpm/v/qex.svg)](https://hex.pm/packages/qex)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/qex/)
[![Total Download](https://img.shields.io/hexpm/dt/qex.svg)](https://hex.pm/packages/qex)
[![License](https://img.shields.io/hexpm/l/qex.svg)](https://github.com/princemaple/elixir-queue/blob/master/LICENSE.md)
[![Last Updated](https://img.shields.io/github/last-commit/princemaple/elixir-queue.svg)](https://github.com/princemaple/elixir-queue/commits/master)

A `:queue` wrapper with improvements in API and addition of Protocol implementations

### Protocols

`Inspect`, `Collectable` and `Enumerable` are implemented,
use Qex with `IO.inspect` and `Enum` functions!

### Function signatures

Parameters are re-ordered to better suit Elixir's awesome `|>`

## Installation

The package can be installed as:

1. Add `:qex` to your list of dependencies in `mix.exs`:

   ```elixir
   def deps do
     [
       {:qex, "~> 0.5"}
     ]
   end
   ```

2. Run `mix deps.get`

## How to use

[Read the docs](https://hexdocs.pm/qex/Qex.html)

#### Protocols

```elixir
iex> inspect Qex.new
"#Qex<[]>"

iex> Enum.count Qex.new(1..5)
5

iex> Enum.empty? Qex.new
true

iex> Enum.map Qex.new([1, 2, 3]), &(&1 + 1)
[2, 3, 4]

iex> inspect Enum.into(1..5, %Qex{})
"#Qex<[1, 2, 3, 4, 5]>"

# Leverages :queue.member/2 under the hood for performance
iex> Enum.member? Qex.new(1..10_000), 9_999
true
```

#### Create a new queue from a range

```elixir
iex> inspect Qex.new(1..3)
"#Qex<[1, 2, 3]>"
```

#### Create a new queue from a list

```elixir
iex> inspect Qex.new([1, 2, 3])
"#Qex<[1, 2, 3]>"
```

#### Add an element to the back of the queue

```elixir
iex> q = Qex.new([:mid])
iex> Enum.to_list Qex.push(q, :back)
[:mid, :back]
```

#### Add an element to the front of the queue

```elixir
iex> q = Qex.new([:mid])
iex> Enum.to_list Qex.push_front(q, :front)
[:front, :mid]
```

#### Get and remove an element from the front of the queue

```elixir
iex> q = Qex.new([:front, :mid])
iex> {{:value, item}, _q} = Qex.pop(q)
iex> item
:front

iex> q = Qex.new
iex> {empty, _q} = Qex.pop(q)
iex> empty
:empty
```

#### Get and remove an element from the back of the queue

```elixir
iex> q = Qex.new([:mid, :back])
iex> {{:value, item}, _q} = Qex.pop_back(q)
iex> item
:back

iex> q = Qex.new
iex> {empty, _q} = Qex.pop_back(q)
iex> empty
:empty
```

#### Reverse a queue

```elixir
iex> q = Qex.new(1..3)
iex> Enum.to_list q
[1, 2, 3]
iex> Enum.to_list Qex.reverse(q)
[3, 2, 1]
```

#### Split a queue into two, the front n items are put in the first queue

```elixir
iex> q = Qex.new 1..5
iex> {q1, q2} = Qex.split(q, 3)
iex> Enum.to_list q1
[1, 2, 3]
iex> Enum.to_list q2
[4, 5]
```

#### Join two queues together

```elixir
iex> q1 = Qex.new 1..3
iex> q2 = Qex.new 4..5
iex> Enum.to_list Qex.join(q1, q2)
[1, 2, 3, 4, 5]
```

#### Return the first item

```elixir
iex> q1 = Qex.new 1..3
iex> Qex.first(q1)
{:value, 1}
iex> q2 = Qex.new []
iex> Qex.first(q2)
:empty

iex> q1 = Qex.new 1..3
iex> Qex.first!(q1)
1
```

#### Return the last item

```elixir
iex> q1 = Qex.new 1..3
iex> Qex.last(q1)
{:value, 3}
iex> q2 = Qex.new []
iex> Qex.last(q2)
:empty

iex> q1 = Qex.new 1..3
iex> Qex.last!(q1)
3
```

## Why not "Queue"?

The name is taken... [Hex link](https://hex.pm/packages/queue)

## Copyright and License

Copyright (c) 2018 Po Chen

This work is free. You can redistribute it and/or modify it under the
terms of the MIT License. See the [LICENSE.md](./LICENSE.md) file for more details.
