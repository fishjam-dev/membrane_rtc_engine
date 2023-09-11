# TODO: add @specs.
# TODO: >, <, >=, <=
defmodule Ratio do
  @vsn "1.2.0"

  @moduledoc """
  This module allows you to use Rational numbers in Elixir, to enable exact calculations with all numbers big and small.

  It also defines the new <|> operator and (optionally) overrides the arithmetic +, -, * and / operators to work with ints, floats and Rational numbers all alike.

  Floats are also automatically coerced into Rationals whenever possible.

  And don't worry: If you don't like operator-overloading: There are longhand function aliases available too.


  To use the module, use `use Ratio` where you need it.

  If you do not want to override the Kernel's built-in math operators, use

      # Does not override *, /, -, +, div, abs
      use Ratio, override_math: false

  If you just do not want to override the Kernel's built-in *inline* math operators, use `use Ratio, inline_math: false`

      # Does not override *, /, -, +
      use Ratio, inline_math: false

  If you do not want the new operator `<|>` to be imported, use

      # Does not include <|>, construct Rational numbers using Ratio.new(a, b)
      use Ratio, operator: false

  These options can be combined (with `override_math` taking precedence over `inline_math` )
  """

  @inline_math_functions [*: 2, /: 2, -: 2, -: 1, +: 2, +: 1]
  # ++ @inline_math_functions
  @overridden_math_functions [div: 2, abs: 1, floor: 1, ceil: 1, trunc: 1]
  @comparison_functions [==: 2, <=: 2, >=: 2, <: 2, >: 2]
  @rational_operator [<|>: 2]
  @never_export_these_functions [to_float: 1, to_float_error: 1, new: 2]

  import Kernel,
    except: [
      div: 2,
      abs: 1,
      floor: 1,
      ceil: 1,
      trunc: 1,
      *: 2,
      /: 2,
      -: 2,
      -: 1,
      +: 2,
      +: 1,
      ==: 2,
      <=: 2,
      >=: 2,
      <: 2,
      >: 2
    ]

  defmacro __using__(opts) do
    override_math = Keyword.get(opts, :override_math, true)
    use_inline_math = Keyword.get(opts, :inline_math, true)
    use_comparison = Keyword.get(opts, :comparison, false)
    use_operator = Keyword.get(opts, :operator, true)

    overridden_kernel_functions =
      cond do
        use_inline_math && override_math ->
          @overridden_math_functions ++ @inline_math_functions

        override_math ->
          @overridden_math_functions

        true ->
          []
      end

    overridden_kernel_functions =
      if use_comparison,
        do: overridden_kernel_functions ++ @comparison_functions,
        else: overridden_kernel_functions

    hidden_functions =
      (@overridden_math_functions ++ @inline_math_functions ++ @comparison_functions) --
        overridden_kernel_functions

    hidden_functions =
      if !use_operator do
        hidden_functions ++ @rational_operator
      else
        hidden_functions
      end

    hidden_functions = hidden_functions ++ @never_export_these_functions

    quote do
      import Kernel, except: unquote(overridden_kernel_functions)
      import Ratio, except: unquote(hidden_functions)
    end
  end

  @doc """
  A Rational number is defined as a numerator and a denominator.
  Both the numerator and the denominator are integers.
  If you want to match for a rational number, you can do so by matching against this Struct.

  Note that *directly manipulating* the struct, however, is usually a bad idea, as then there are no validity checks, nor wil the rational be simplified.

  Use `Ratio.<|>/2` or `Ratio.new/2` instead.
  """
  defstruct numerator: 0, denominator: 1
  @type t :: %Ratio{numerator: integer(), denominator: pos_integer()}

  @doc """
  Creates a new Rational number.
  This number is simplified to the most basic form automatically.
  If the most basic form has the format `_ <|> 1`, it is returned in integer form.

  Rational numbers with a `0` as denominator are not allowed.

  Note that it is recommended to use integer numbers for the numerator and the denominator.

  ## Floats

  Tl;Dr: *If possible, don't use them.*

  Using Floats for the numerator or denominator is possible, however, because base-2 floats cannot represent all base-10 fractions properly, the results might be different from what you might expect.
  See [The Perils of Floating Point](http://www.lahey.com/float.htm) for more information about this.

  Passed floats are rounded to `#{Application.get_env(:ratio, :max_float_to_rational_digits)}` digits, to make the result match expectations better.
  This number can be changed by adding `max_float_to_rational_digits: 10` to your config file.

  See `Ratio.FloatConversion.float_to_rational/2` for more info about float -> rational parsing.

  As Float-parsing is done by converting floats to a digit-list representation first, this is also far slower than when using integers or rationals.

  ## Decimals

  To use `Decimal` parameters, the [decimal](https://hex.pm/packages/decimal) library must
  be configured in `mix.exs`.

  ## Examples

      iex> 1 <|> 2
      1 <|> 2
      iex> 100 <|> 300
      1 <|> 3
      iex> 1.5 <|> 4
      3 <|> 8
  """
  def numerator <|> denominator

  def _numerator <|> 0 do
    raise ArithmeticError
  end

  def numerator <|> denominator when is_integer(numerator) and is_integer(denominator) do
    %Ratio{numerator: numerator, denominator: denominator}
    |> simplify
    |> remove_denominator_if_integer
  end

  def numerator <|> denominator when is_float(numerator) do
    div(Ratio.FloatConversion.float_to_rational(numerator), denominator)
  end

  def numerator <|> denominator when is_float(denominator) do
    div(numerator, Ratio.FloatConversion.float_to_rational(denominator))
  end

  def (numerator = %Ratio{}) <|> (denominator = %Ratio{}) do
    div(numerator, denominator)
  end

  if Code.ensure_loaded?(Decimal) do
    def (numerator = %Decimal{}) <|> (denominator = %Decimal{}) do
      Ratio.DecimalConversion.decimal_to_rational(numerator)
      |> div(Ratio.DecimalConversion.decimal_to_rational(denominator))
    end

    def (numerator = %Decimal{}) <|> denominator when is_float(denominator) do
      Ratio.DecimalConversion.decimal_to_rational(numerator)
      |> div(Ratio.FloatConversion.float_to_rational(denominator))
    end

    def numerator <|> (denominator = %Decimal{}) when is_float(numerator) do
      Ratio.FloatConversion.float_to_rational(numerator)
      |> div(Ratio.DecimalConversion.decimal_to_rational(denominator))
    end

    def (numerator = %Decimal{}) <|> denominator when is_integer(denominator) do
      Ratio.DecimalConversion.decimal_to_rational(numerator)
      |> div(denominator)
    end

    def numerator <|> (denominator = %Decimal{}) when is_integer(numerator) do
      div(Ratio.DecimalConversion.decimal_to_rational(numerator), denominator)
    end
  end

  def numerator <|> denominator do
    div(numerator, denominator)
  end

  @doc """
  Prefix-version of `numerator <|> denominator`.
  Useful when `<|>` is not available (for instance, when already in use by another module)

  Not imported when calling `use Ratio`, so always call it as `Ratio.new(a, b)`

  To use `Decimal` parameters, the [decimal](https://hex.pm/packages/decimal) library must
  be configured in `mix.exs`.

  ## Examples

      iex> Ratio.new(1, 2)
      1 <|> 2
      iex> Ratio.new(100, 300)
      1 <|> 3
      iex> Ratio.new(1.5, 4)
      3 <|> 8
      iex> Ratio.new(Decimal.new("123.456"))
      15432 <|> 125

  """
  def new(numerator, denominator \\ 1)

  if Code.ensure_loaded?(Decimal) do
    def new(%Decimal{} = decimal, 1) do
      Ratio.DecimalConversion.decimal_to_rational(decimal)
    end

    def new(%Decimal{} = numerator, %Decimal{} = denominator) do
      Ratio.DecimalConversion.decimal_to_rational(numerator) <|>
      Ratio.DecimalConversion.decimal_to_rational(denominator)
    end

    def new(numerator, %Decimal{} = denominator) do
      numerator <|> Ratio.DecimalConversion.decimal_to_rational(denominator)
    end
  end

  def new(numerator, denominator) do
    numerator <|> denominator
  end

  @doc """
  Returns the absolute version of the given number (which might be an integer, float or Rational).

  ## Examples

      iex>Ratio.abs(-5 <|> 2)
      5 <|> 2
  """
  def abs(number) when is_number(number), do: Kernel.abs(number)

  def abs(%Ratio{numerator: numerator, denominator: denominator}),
    do: Kernel.abs(numerator) <|> denominator

  @doc """
  Returns the sign of the given number (which might be an integer, float or Rational)

  This is:

   - 1 if the number is positive.
   - -1 if the number is negative.
   - 0 if the number is zero.

  """
  def sign(%Ratio{numerator: numerator}) when Kernel.>(numerator, 0), do: 1
  def sign(%Ratio{numerator: numerator}) when Kernel.<(numerator, 0), do: Kernel.-(1)
  def sign(number) when is_number(number) and Kernel.>(number, 0), do: 1
  def sign(number) when is_number(number) and Kernel.<(number, 0), do: Kernel.-(1)
  def sign(number) when is_number(number), do: 0

  @doc """
  Converts the passed *number* as a Rational number, and extracts its denominator.
  For integers returns the passed number itself.

  """
  def numerator(number) when is_integer(number), do: number

  def numerator(number) when is_float(number),
    do: numerator(Ratio.FloatConversion.float_to_rational(number))

  def numerator(%Ratio{numerator: numerator}), do: numerator

  @doc """
  Treats the passed *number* as a Rational number, and extracts its denominator.
  For integers, returns `1`.
  """
  def denominator(number) when is_number(number), do: 1
  def denominator(%Ratio{denominator: denominator}), do: denominator

  @doc """
  Longhand for `Ratio.+/2`
  """
  def add(a, b)

  def add(a, b) when is_integer(a) and is_integer(b), do: Kernel.+(a, b)

  def add(a, b) when is_float(a), do: add(Ratio.FloatConversion.float_to_rational(a), b)

  def add(a, b) when is_float(b), do: add(a, Ratio.FloatConversion.float_to_rational(b))

  def add(a, %Ratio{numerator: b, denominator: lcm}) when is_integer(a),
    do: Kernel.+(a * lcm, b) <|> lcm

  def add(%Ratio{numerator: a, denominator: lcm}, b) when is_integer(b),
    do: Kernel.+(b * lcm, a) <|> lcm

  def add(%Ratio{numerator: a, denominator: lcm}, %Ratio{numerator: c, denominator: lcm}) do
    Kernel.+(a, c) <|> lcm
  end

  def add(%Ratio{numerator: a, denominator: b}, %Ratio{numerator: c, denominator: d}) do
    Kernel.+(a * d, c * b) <|> (b * d)
  end

  @doc """
  Adds two numbers, one or both of which might be integers, floats or rationals.

  The result is converted to a rational if applicable.

  ## Examples

      iex> 2 + 3
      5
      iex> 2.3 + 0.3
      13 <|> 5
      iex> 2 + (2 <|> 3)
      8 <|> 3
  """
  def a + b when is_integer(a) and is_integer(b), do: Kernel.+(a, b)
  def a + b, do: add(a, b)

  @doc """
  Longhand for `Ratio.-/2`
  """
  def sub(a, b) when is_integer(a) and is_integer(b), do: Kernel.-(a, b)
  def sub(a, b), do: add(a, negate(b))

  @doc """
  Subtracts *b* from *a*. One or both might be integers, floats or rationals.

  The result is converted to a rational if applicable.

  ## Examples

      iex> 2 - 3
      -1
      iex> 2.3 - 0.3
      2
      iex> 2.3 - 0.1
      11 <|> 5
      iex> (2 <|> 3) - (1 <|> 5)
      7 <|> 15
  """
  def a - b when is_integer(a) and is_integer(b), do: Kernel.-(a, b)
  def a - b, do: add(a, negate(b))

  @doc """
  Longhand for `Ratio.-/1`
  """
  def negate(num)

  def negate(num) when is_integer(num), do: Kernel.-(num)

  def negate(num) when is_float(num), do: negate(Ratio.FloatConversion.float_to_rational(num))

  def negate(%Ratio{numerator: numerator, denominator: denominator}) do
    %Ratio{numerator: Kernel.-(numerator), denominator: denominator}
  end

  @doc """
  Alias for `Ratio.negate(num)`; follows Numeric behaviour.
  """
  def minus(num), do: negate(num)

  @doc """
  Unary minus. Inverts the sign of the given *num*, which might be an integer, float or rational.
  Floats are converted to Rationals before inverting the sign.


  ## Examples

      iex> -10
      -10
      iex> -10.0
      -10
      iex> -10.1
      -101 <|> 10
      iex> -(5 <|> 3)
      -5 <|> 3
      iex> -123.456
      -15432 <|> 125
  """
  def -num when is_integer(num), do: Kernel.-(num)

  def -num, do: negate(num)

  @doc """
  Unary plus. Returns *num*.
  Coerces the number to a rational if it is a float.
  """
  def +num when is_integer(num), do: Kernel.+(num)
  def +num when is_float(num), do: Ratio.FloatConversion.float_to_rational(num)
  def +num, do: num

  @doc """
  Longhand for `Ratio.*/2`
  """
  def mult(number1, number2)

  def mult(number1, number2) when is_number(number1) and is_number(number2),
    do: Kernel.*(number1, number2)

  def mult(%Ratio{numerator: numerator, denominator: denominator}, number)
      when is_number(number) do
    Kernel.*(numerator, number) <|> denominator
  end

  def mult(number, %Ratio{numerator: numerator, denominator: denominator})
      when is_number(number) do
    Kernel.*(numerator, number) <|> denominator
  end

  def mult(%Ratio{numerator: numerator1, denominator: denominator1}, %Ratio{
        numerator: numerator2,
        denominator: denominator2
      }) do
    Kernel.*(numerator1, numerator2) <|> Kernel.*(denominator1, denominator2)
  end

  @doc false
  # TODO Remove in future version.
  def mul(number1, number2) do
    IO.puts("Warning: `Ratio.mul/2` is deprecated. Use `Ratio.mult/2` instead.")
    mult(number1, number2)
  end

  @doc """
  Multiplies two numbers. (one or both of which might be integers, floats or rationals)

  ## Examples

      iex> ((2 <|> 3) *  10)
      20 <|> 3
      iex> ( 1 <|> 3) * (1 <|> 2)
      1 <|> 6
  """
  def a * b

  def a * b when is_number(a) and is_number(b), do: Kernel.*(a, b)

  def a * b, do: mult(a, b)

  @doc """
  Longhand for `Ratio.//2`

  """
  def div(a, b)

  def div(a, b) when is_number(a) and is_integer(b), do: a <|> b

  def div(a, b) when is_number(a) and is_float(b),
    do: div(a, Ratio.FloatConversion.float_to_rational(b))

  def div(%Ratio{numerator: numerator, denominator: denominator}, number)
      when is_number(number) do
    numerator <|> Kernel.*(denominator, number)
  end

  # 6 / (2 <|> 3) == 6 * (3 <|> 2)
  def div(number, %Ratio{numerator: numerator, denominator: denominator})
      when is_number(number) do
    mult(number, denominator <|> numerator)
  end

  def div(%Ratio{numerator: numerator1, denominator: denominator1}, %Ratio{
        numerator: numerator2,
        denominator: denominator2
      }) do
    Kernel.*(numerator1, denominator2) <|> Kernel.*(denominator1, numerator2)
  end

  @doc """
  Divides a number by another number, one or both of which might be integers, floats or rationals.

  The function will return integers whenever possible, and otherwise returns a rational number.

  ## Examples

      iex> (1 <|> 3) / 2
      1 <|> 6
      iex> (2 <|> 3) / (8 <|> 5)
      5 <|> 12
      iex> 2.0 / 1.0
      2

  """
  def a / b

  def a / b when is_number(a) and is_integer(b), do: a <|> b

  def a / b, do: div(a, b)

  defmodule ComparisonError do
    defexception message: "These things cannot be compared."
  end

  def compare(%Ratio{numerator: a, denominator: b}, %Ratio{numerator: c, denominator: d}) do
    compare(Kernel.*(a, d), Kernel.*(b, c))
  end

  def compare(%Ratio{numerator: numerator, denominator: denominator}, b) do
    compare(numerator, Kernel.*(b, denominator))
  end

  def compare(a, %Ratio{numerator: numerator, denominator: denominator}) do
    compare(Kernel.*(a, denominator), numerator)
  end

  # Compares any other value that Elixir/Erlang can understand.
  def compare(a, b) do
    cond do
      Kernel.>(a, b) -> :gt
      Kernel.<(a, b) -> :lt
      Kernel.==(a, b) -> :eq
      true -> raise ComparisonError, "These things cannot be compared: #{a} , #{b}"
    end
  end

  @doc """
  True if *a* is equal to *b*
  """
  def eq?(a, b), do: compare(a, b) |> Kernel.==(:eq)

  @doc """
  True if *a* is larger than or equal to *b*
  """
  def gt?(a, b), do: compare(a, b) |> Kernel.==(:gt)

  @doc """
  True if *a* is smaller than *b*
  """
  def lt?(a, b), do: compare(a, b) |> Kernel.==(:lt)

  @doc """
  True if *a* is larger than or equal to *b*
  """
  def gte?(a, b), do: compare(a, b) in [:eq, :gt]

  @doc """
  True if *a* is smaller than or equal to *b*
  """
  def lte?(a, b), do: compare(a, b) in [:lt, :eq]

  @doc """
  True if *a* is equal to *b*?
  """
  def equal?(a, b), do: compare(a, b) |> Kernel.==(:eq)

  @doc """
  Compares two numbers and returns true if the first equal to the second.

  ## Examples

    iex> 2 == 3
    false
    iex> 5 == 5
    true
    iex> 2.3 == 0.3
    false
    iex> 0.1 == (1 <|> 10)
    true
  """
  def a == b, do: eq?(a, b)

  @doc """
  Compares two numbers and returns true if the first is less than the second.

  ## Examples

      iex> 2 < 3
      true
      iex> 5 < 5
      false
      iex> 2.3 < 0.3
      false
      iex> 10 < (1 <|> 10)
      false
  """
  def a < b, do: lt?(a, b)

  @doc """
  Compares two numbers and returns true if the first is less than or equal to the second.

  ## Examples

      iex> 2 <= 3
      true
      iex> 5 <= 5
      true
      iex> 2.3 <= 0.3
      false
      iex> 10 <= (1 <|> 10)
      false
  """
  def a <= b, do: lte?(a, b)

  @doc """
  Compares two numbers and returns true if the first is greater than the second.

  ## Examples

      iex> 2 > 3
      false
      iex> 5 > 5
      false
      iex> 2.3 > 0.3
      true
      iex> 10 > (1 <|> 10)
      true
  """
  def a > b, do: gt?(a, b)

  @doc """
  Compares two numbers and returns true if the first is greater than or equal to the second.

  ## Examples

      iex> 2 >= 3
      false
      iex> 5 >= 5
      true
      iex> 2.3 >= 0.3
      true
      iex> 10 >= (1 <|> 10)
      true
  """
  def a >= b, do: gte?(a, b)

  @doc """
  returns *x* to the *n* th power.

  *x* is allowed to be an integer, rational or float (in the last case, this is first converted to a rational).

  Will give the answer as a rational number when applicable.
  Note that the exponent *n* is only allowed to be an integer.

  (so it is not possible to compute roots using this function.)

  ## Examples

      iex>pow(2, 4)
      16
      iex>pow(2, -4)
      1 <|> 16
      iex>pow(3 <|> 2, 10)
      59049 <|> 1024
  """
  @spec pow(number() | Ratio.t(), pos_integer()) :: number() | Ratio.t()
  def pow(x, n)

  # Convert Float to Rational.
  def pow(x, n) when is_float(x), do: pow(Ratio.FloatConversion.float_to_rational(x), n)

  # Small powers
  def pow(x, 1), do: x
  def pow(x, 2), do: x * x
  def pow(x, 3), do: x * x * x
  def pow(x, n) when is_integer(n), do: do_pow(x, n)

  # Exponentiation By Squaring.
  defp do_pow(x, n, y \\ 1)
  defp do_pow(_x, 0, y), do: y
  defp do_pow(x, 1, y), do: x * y
  defp do_pow(x, n, y) when Kernel.<(n, 0), do: do_pow(1 / x, Kernel.-(n), y)
  defp do_pow(x, n, y) when rem(n, 2) |> Kernel.==(0), do: do_pow(x * x, div(n, 2), y)
  defp do_pow(x, n, y), do: do_pow(x * x, div(n - 1, 2), x * y)

  @doc """
  Converts the given *number* to a Float. As floats do not have arbitrary precision, this operation is generally not reversible.

  Not imported when calling `use Ratio`, so always call it as `Rational.to_float(number)`
  """
  @spec to_float(Ratio.t() | number) :: float
  def to_float(%Ratio{numerator: numerator, denominator: denominator}),
    do: Kernel./(numerator, denominator)

  def to_float(number), do: :erlang.float(number)

  @doc """
  Returns a tuple, where the first element is the result of `to_float(number)` and
  the second is a conversion error.

  The conversion error is calculated by subtracting the original number from the
  conversion result.

  ## Examples

      iex> Ratio.to_float_error(Ratio.new(1, 2))
      {0.5, 0}
      iex> Ratio.to_float_error(Ratio.new(2, 3))
      {0.6666666666666666, 1 <|> 30000000000}
  """
  @spec to_float_error(t | number) :: {float, error} when error: t | number
  def to_float_error(number) do
    float = to_float(number)
    {float, float - number}
  end

  @doc """
  Check if a number is a rational number.
  Returns false if the number is an integer, float or any other type.

  To check if a float representation will result in a rational number, combine it with the unary plus operation:

  ## Examples

      iex>Ratio.is_rational?(10)
      false
      iex>Ratio.is_rational?("foo")
      false
      iex>Ratio.is_rational?(10.0)
      false
      iex>Ratio.is_rational?(10.234)
      false
      iex>Ratio.is_rational?(10 <|> 3)
      true
      iex>Ratio.is_rational?(10 <|> 5)
      false
      iex>Ratio.is_rational?(+20.234)
      true
      iex>Ratio.is_rational?(+20.0)
      false

  """
  def is_rational?(%Ratio{}), do: true
  def is_rational?(_), do: false

  @doc """
  Returns a binstring representation of the Rational number.
  If the denominator is `1`, it will be printed as a normal (integer) number.

  ## Examples

      iex> Ratio.to_string 10 <|> 7
      "10 <|> 7"
  """
  def to_string(rational)

  def to_string(%Ratio{numerator: numerator, denominator: denominator})
      when denominator |> Kernel.==(1) do
    "#{numerator}"
  end

  def to_string(%Ratio{numerator: numerator, denominator: denominator}) do
    "#{numerator} <|> #{denominator}"
  end

  defimpl String.Chars, for: Ratio do
    def to_string(rational) do
      Ratio.to_string(rational)
    end
  end

  defimpl Inspect, for: Ratio do
    def inspect(rational, _) do
      Ratio.to_string(rational)
    end
  end

  # Simplifies the Rational to its most basic form.
  # Which might result in an integer.
  # Ensures that a `-` is only kept in the numerator.
  defp simplify(rational)

  defp simplify(%Ratio{numerator: numerator, denominator: denominator}) do
    gcdiv = gcd(numerator, denominator)
    new_denominator = Kernel.div(denominator, gcdiv)
    {new_denominator, numerator} = normalize_denom_num(new_denominator, numerator)

    if new_denominator == 1 do
      Kernel.div(numerator, gcdiv)
    else
      %Ratio{numerator: Kernel.div(numerator, gcdiv), denominator: new_denominator}
    end
  end

  defp normalize_denom_num(denominator, numerator) do
    if denominator < 0 do
      {Kernel.-(denominator), Kernel.-(numerator)}
    else
      {denominator, numerator}
    end
  end

  # Returns an integer if the result is of the form _ <|> 1
  defp remove_denominator_if_integer(rational)
  defp remove_denominator_if_integer(%Ratio{numerator: numerator, denominator: 1}), do: numerator
  defp remove_denominator_if_integer(rational), do: rational

  # Calculates the Greatest Common denominator of two numbers.
  defp gcd(a, 0), do: abs(a)

  defp gcd(0, b), do: abs(b)
  defp gcd(a, b), do: gcd(b, Kernel.rem(a, b))

  @doc """
  Rounds a number (rational, integer or float) to the largest whole number less than or equal to num.
  For negative numbers, this means we are rounding towards negative infinity.


  iex> Ratio.floor(Ratio.new(1, 2))
  0
  iex> Ratio.floor(Ratio.new(5, 4))
  1
  iex> Ratio.floor(Ratio.new(-3, 2))
  -2

  """
  def floor(num) when is_integer(num), do: num
  def floor(num) when is_float(num), do: Float.floor(num)

  def floor(%Ratio{numerator: numerator, denominator: denominator}),
    do: Integer.floor_div(numerator, denominator)

  @doc """
  Rounds a number (rational, integer or float) to the largest whole number larger than or equal to num.
  For negative numbers, this means we are rounding towards negative infinity.


  iex> Ratio.ceil(Ratio.new(1, 2))
  1
  iex> Ratio.ceil(Ratio.new(5, 4))
  2
  iex> Ratio.ceil(Ratio.new(-3, 2))
  -1

  """
  def ceil(num) when is_float(num), do: Float.ceil(num)
  def ceil(num) when is_integer(num), do: num

  def ceil(num = %Ratio{numerator: numerator, denominator: denominator}) do
    floor = floor(num)

    if numerator <|> denominator == floor do
      floor
    else
      floor + 1
    end
  end

  @doc """
  Returns the integer part of number.

  ## Examples

      iex> Ratio.trunc(1.7)
      1
      iex> Ratio.trunc(-1.7)
      -1
      iex> Ratio.trunc(3)
      3
      iex> Ratio.trunc(Ratio.new(5, 2))
      2
  """
  @spec trunc(t | number) :: integer
  def trunc(num) when is_integer(num), do: num
  def trunc(num) when is_float(num), do: Kernel.trunc(num)

  def trunc(%Ratio{numerator: numerator, denominator: denominator}) do
    Kernel.div(numerator, denominator)
  end

  # So they can without problem be overridden by other libraries that extend on this one.
  defoverridable @overridden_math_functions
end
