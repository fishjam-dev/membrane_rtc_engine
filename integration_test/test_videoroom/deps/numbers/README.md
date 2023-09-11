# Numbers
[![hex.pm version](https://img.shields.io/hexpm/v/numbers.svg)](https://hex.pm/packages/numbers)
[![Build Status](https://travis-ci.org/Qqwy/elixir-number.svg?branch=master)](https://travis-ci.org/Qqwy/elixir-number)
[![Inline docs](http://inch-ci.org/github/qqwy/elixir_number.svg)](http://inch-ci.org/github/qqwy/elixir_number)


**Numbers** is a tiny Elixir package that facilitates the creation of libraries
that want to be able to use _any_ kind of Numberlike type.

Some known custom numeric types that implement Numbers' protocols:

- [Ratio](https://hex.pm/packages/ratio) -- rational numbers.
- [Decimal](https://hex.pm/packages/decimal) -- arbitrary precision decimal numbers.
- [Tensor](https://hex.pm/packages/tensor) -- Vectors, Matrices and higher-order tensors.
- [ComplexNum](https://github.com/Qqwy/elixir_complex_num) -- Complex numbers.

Just add one (or multiple) of these libraries to your project, together with Numbers, and you're good to go!

## How does it work?

Starting at version 5, Numbers contains a set of protocols that can be independently implemented for your data structures.

Each protocol maps to a single arithmetical operation that your data structure might support.

Because protocols are used, Numbers can dispatch quite fast!
Also, Numbers does not restrict your modules to any special naming schemes (as was the case with older versions of Numbers that used a Behaviour).

The following operations are supported:

- `add` for addition, by implementing `Numbers.Protocols.Addition`.
- `sub` for subtraction, by implementing `Numbers.Protocols.Subtraction`.
- `mult` for multiplication, by implementing `Numbers.Protocols.Multiplication`.
- `div` for division, by implementing `Numbers.Protocols.Division`.
- `minus` for unary minus (negation), by implementing `Numbers.Protocols.Minus`.
- `abs` to calculate the absolute value of a number, by implementing `Numbers.Protocols.Absolute`.
- `pow` for calculating integer powers, by implementing `Numbers.Protocols.Exponentiation`. A special helper in `Numbers.Helpers.pow_by_sq` can be used inside this implementation to automatically make use of the _'Exponentiation by Squaring'_ algorithm.
- `to_float` for (possibly lossy) conversion to the built-in Float datatype, by implementing `Numbers.Protocols.ToFloat`.

## Coercion

Numbers does not automatically transform numbers from one type to another if one of the functions is called with two different types.

Frequently you do want to use other data types together with your custom data type. For this, a custom coercion can be specified,
using `Coerce.defcoercion` as exposed by the [`Coerce`](https://hex.pm/packages/coerce) library that `Numbers` depends on.

The only coercion that ships with Numbers itself, is a coercion of Integers to Floats, meaning that they work the same way as when using
the standard library math functions with these types.

## Overloaded Operators

You can opt-in to overloaded `+, -, *, /` operators by calling `use Numbers, overload_operators: true`.
This allows you to use these inline operators for all other Numberlike types.

The library uses a conditional compilation technique to make sure that 
**you will _still_ be able to use the operators inside guards** for built-in integers and floats.

As example consider:

```elixir
defmodule An.Example do
  use Numbers, overload_operators: true

  def foo(a, b) when a + b < 10 do  # Uses the normal guard-safe '+' operator (e.g. Kernel.+/2)
    42
  end
  def foo(c, d) do 
    c + d # Uses the overloaded '+' operator.
  end
end
```

## Examples:

Using built-in numbers:

```elixir
iex> alias Numbers, as: N

iex> N.add(1, 2)
3

iex> N.mult(3,5)
15

iex> N.mult(1.5, 100)
150.0
```

Using Decimals: (requires the [Decimal](https://hex.pm/packages/decimal) library.)

```elixir
iex> alias Numbers, as: N

iex> d = Decimal.new(2)
iex> N.div(d, 10)
#Decimal<0.2>
iex> small_number = N.div(d, 1234)
#Decimal<0.001620745542949756888168557536>
iex> N.pow(small_number, 100)

```


## Installation

The package can be installed as:

1. Add `numbers` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:numbers, "~> 5.2"}]
end
```

## Changelog

- 5.2.4 Updates `:decimal` dependency to now allow both version `1.x` as well as version `2.x`.
- 5.2.3 Updates `:decimal` dependency to `1.9` or newer, and replaces deprecated `Decimal.minus/1` call with `Decimal.negate/1`
- 5.2.2 Updated `mix.exs` to use `extra_applications` rather than manually overridding `applications`. This drops support for now very old versions of Elixir (< v1.4) but ensures proper support with Elixir v1.11 and beyond.
- 5.2.1 Better error message when wrongly using an operator at the left side of a match (e.g. `a - 1 = 10`).
- 5.2.0 Ensures that overloaded operators do not prevent built-in operators to be used in guards.
- 5.1.1 Makes `Decimal` dependency version less specific to play nicer with other libraries :-).
- 5.1.0 Possibility to import overloaded operator variants. Also, greatly improved documentation.
- 5.0.0 MAJOR OVERHAUL: New implementation based on a set of Protocols. Should be a lot faster and easier on implementers. Also uses a new method to perform coercions based on the `Coerce` library. [Announcement post](https://elixirforum.com/t/numbers-a-generic-wrapper-to-use-any-custom-numeric-type/2846/7)
- 4.0.0 Breaking change: Move `Numeric` to `Numbers.Numeric`, to follow proper code organization conventions.
- 3.0.1 Improved README
- 3.0.0 Remove public `Numbers.coerce/2` function, as it had confused naming and very limited use. Added optional `Numeric.coerce/2` callback (which works very different from the old `Numbers.coerce/2` function) which is now used underwater when coercion should happen.
- 2.0.3 Improving documentation.
- 2.0.2 Adding many tests.
- 2.0.1 Fixing error message that is shown when conversion to float is not possible to use the Inspect protocol. 
- 2.0.0 Breaking change, `mul` -> `mult`. 
- 1.0.0 First Stable Version.
