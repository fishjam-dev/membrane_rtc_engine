defmodule Numbers.Helper do
  @moduledoc """
  Helper functions that might make the implementation
  of Numbers for your own numberlike types easier.
  """

  @doc """
  Performs 'Exponentiation by Squaring',
  which is a reasonably fast algorithm to compute integer powers,
  by performing log(n) multiplication steps.

  Depends on an implementation existing of `Numbers.Protocols.Multiplication`,
  as well as (to support negative powers) an implementation of `Numbers.Protocols.Division`.
  """
  def pow_by_sq(num, integer_power)
  # Small powers
  def pow_by_sq(x, 1), do: x
  def pow_by_sq(x, 2), do: Numbers.Protocols.Multiplication.mult(x, x)
  def pow_by_sq(x, 3), do: Numbers.Protocols.Multiplication.mult(Numbers.Protocols.Multiplication.mult(x, x), x)
  def pow_by_sq(x, n) when is_integer(n), do: do_pow_by_sq(x, n)

  # Exponentiation By Squaring.
  defp do_pow_by_sq(x, n, y \\ 1)
  defp do_pow_by_sq(_x, 0, y), do: y
  defp do_pow_by_sq(x, 1, y), do: Numbers.Protocols.Multiplication.mult(x, y)
  defp do_pow_by_sq(x, n, y) when n < 0, do: do_pow_by_sq(Numbers.Protocols.Division.div(Numbers.Protocols.Multiplication.mult_id(x), x), Kernel.-(n), y)
  defp do_pow_by_sq(x, n, y) when rem(n, 2) == 0, do: do_pow_by_sq(Numbers.Protocols.Multiplication.mult(x, x), Kernel.div(n, 2), y)
  defp do_pow_by_sq(x, n, y), do: do_pow_by_sq(Numbers.Protocols.Multiplication.mult(x, x), Kernel.div((n - 1), 2), Numbers.mult(x, y))
end
