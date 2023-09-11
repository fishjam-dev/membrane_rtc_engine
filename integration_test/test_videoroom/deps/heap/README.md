# Heap
[![Build Status](https://travis-ci.org/jamesotron/heap.svg?branch=master)](https://travis-ci.org/jamesotron/heap)
[![Hex.pm](https://img.shields.io/hexpm/v/heap.svg)](https://hex.pm/packages/heap)
[![Inline docs](http://inch-ci.org/github/jamesotron/heap.svg)](http://inch-ci.org/github/jamesotron/heap)

A Heap is a very useful data structure, because it sorts, quickly, at insert time.


See also: https://en.wikipedia.org/wiki/Heap_(data_structure)

You can use it for things like:

  - Help with scientific computing
  - Quickly sorting
  - Priority queues

## Installation

This package is [available in Hex](https://hex.pm/packages/heap):

  1. Add heap to your list of dependencies in `mix.exs`:

        ```elixir
        def deps do
          [{:heap, "~> 2.0"}]
        end
        ```

  2. Run `mix deps.get`

## Deprecation warning

If you're upgrading from `heap` < 2.0 be aware that the direction of the `:<`
and `:>` atoms passed into `Heap.new/1` has changed to make more sense.

## Examples

Create a min heap and use it to find the smallest element in a collection:

```elixir
1..500 |> Enum.shuffle |> Enum.into(Heap.min) |> Heap.root
# => 1
```

Likewise, for max heaps:

```elixir
1..500 |> Enum.shuffle |> Enum.into(Heap.max) |> Heap.root
# => 500
```

A priority queue:

Tuples are compared by their elements in order, so you can push tuples
of `{priority, term}` into a Heap for sorting by priority:

```elixir
Heap.new
|> Heap.push({4, :jam})
|> Heap.push({1, :milk})
|> Heap.push({2, :eggs})
|> Heap.push({1, :bread})
|> Heap.push({3, :butter})
|> Heap.push({2, :coffee})
|> Enum.map(fn {_, what} -> what end)
# => [:bread, :milk, :coffee, :eggs, :butter, :jam]
```

The heap can also be constructed with a custom comparator:

```elixir
Heap.new(&(Date.compare(elem(&1, 0), elem(&2, 0)) == :gt))
|> Heap.push({~D[2017-11-20], :jam})
|> Heap.push({~D[2017-11-21], :milk})
|> Heap.push({~D[2017-10-21], :bread})
|> Heap.push({~D[2017-10-20], :eggs})
|> Enum.map(fn {_, what} -> what end)
# => [:milk, :jam, :bread, :eggs]
```

To access the root and the rest of the heap in one line use `Heap.split/1`:

```elixir
{root, rest} = Heap.split(heap)
{root, rest} == {Heap.root(heap), Heap.pop(heap)}
# => true
```

### Documentation

Full API documentation is available on (hexdocs.pm)[https://hexdocs.pm/heap]

## Contributing

1. Fork it ( https://github.com/jamesotron/heap/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
