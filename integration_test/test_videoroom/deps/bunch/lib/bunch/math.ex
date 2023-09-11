defmodule Bunch.Math do
  @moduledoc """
  A bunch of math helper functions.
  """

  @doc """
  Applies `div/2` and `rem/2` to arguments and returns results as a tuple.

  ## Example

      iex> #{inspect(__MODULE__)}.div_rem(10, 4)
      {div(10, 4), rem(10, 4)}

  """
  @spec div_rem(divident :: non_neg_integer, divisor :: pos_integer) ::
          {div :: non_neg_integer, rem :: non_neg_integer}
  def div_rem(dividend, divisor) do
    {div(dividend, divisor), rem(dividend, divisor)}
  end

  @doc """
  Works like `div_rem/2` but allows to accumulate remainder.

  Useful when an accumulation of division error is not acceptable, for example
  when you need to produce chunks of data every second but need to make sure there
  are 9 chunks per 4 seconds on average. You can calculate `div_rem(9, 4)`,
  keep the remainder, pass it to subsequent calls and every fourth result will be
  bigger than others.

  ## Example

      iex> 1..10 |> Enum.map_reduce(0, fn _, err ->
      ...>  #{inspect(__MODULE__)}.div_rem(9, 4, err)
      ...>  end)
      {[2, 2, 2, 3, 2, 2, 2, 3, 2, 2], 2}

  """
  @spec div_rem(
          divident :: non_neg_integer,
          divisor :: pos_integer,
          accumulated_remainder :: non_neg_integer
        ) :: {div :: non_neg_integer, rem :: non_neg_integer}
  def div_rem(dividend, divisor, accumulated_remainder) do
    div_rem(accumulated_remainder + dividend, divisor)
  end

  @doc """
  Returns the biggest multiple of `value` that is lower than or equal to `threshold`.

  ## Examples

      iex> #{inspect(__MODULE__)}.max_multiple_lte(4, 10)
      8
      iex> #{inspect(__MODULE__)}.max_multiple_lte(2, 6)
      6

  """
  @spec max_multiple_lte(value :: pos_integer, threshold :: non_neg_integer) :: non_neg_integer
  def max_multiple_lte(value, threshold) do
    remainder = threshold |> rem(value)
    threshold - remainder
  end

  @doc """
  Returns the smallest multiple of `value` that is greater than or equal to `threshold`.

  ## Examples

      iex> #{inspect(__MODULE__)}.min_multiple_gte(4, 10)
      12
      iex> #{inspect(__MODULE__)}.min_multiple_gte(2, 6)
      6

  """
  @spec min_multiple_gte(value :: pos_integer, threshold :: non_neg_integer) :: non_neg_integer
  def min_multiple_gte(value, threshold) do
    case threshold |> rem(value) do
      0 -> threshold
      remainder -> threshold + value - remainder
    end
  end
end
