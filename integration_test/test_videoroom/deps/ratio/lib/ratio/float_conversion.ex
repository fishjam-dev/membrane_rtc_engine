defmodule Ratio.FloatConversion do
  use Ratio

  @max_decimals Application.get_env(:ratio, :max_float_to_rational_digits)

  @doc """
  Converts a float to a rational number.
  Because base-2 floats cannot represent all base-10 fractions properly, the results might be different from what you might expect.
  See [The Perils of Floating Point](http://www.lahey.com/float.htm) for more information about this.

  It is possible to restrict *max_decimals* to make the result more readable/understandable, at the possible loss of precision.
  The default value for *max_decimals* is `#{@max_decimals}` (Erlang allows values between 0 and 249, see erlang's `float_to_list` function)

  ## Examples

      iex> Ratio.FloatConversion.float_to_rational(10.0)
      10
      iex> Ratio.FloatConversion.float_to_rational(13.5)
      27 <|> 2
      iex> Ratio.FloatConversion.float_to_rational(1.1, 100)
      2476979795053773 <|> 2251799813685248
      iex> Ratio.FloatConversion.float_to_rational(1.1, 3)
      11 <|> 10

  """
  def float_to_rational(float, max_decimals \\ @max_decimals)

  def float_to_rational(float, max_decimals) when Kernel.<(float, 0.0) do
    -float_to_rational(abs(float), max_decimals)
  end

  def float_to_rational(float, max_decimals) do
    float_to_intdec_tuple(float, max_decimals)
    |> intdec_tuple_to_rational
  end

  # Changes 1.234 to {'1', '234'}
  defp float_to_intdec_tuple(float, max_decimals) do
    # While the `:decimals` option is allowed to be 0..249 according to the Erlang documentation,
    # it will throw errors on large numbers if you do.
    {integer_list, [?. | decimal_list]} =
      :erlang.float_to_list(float, [{:decimals, max_decimals}, :compact])
      |> Enum.split_while(fn x -> x != ?. end)

    {integer_list, decimal_list}
  end

  # Changes {'1', '234'} to (1234 <|> 1000)
  defp intdec_tuple_to_rational({integer_list, decimal_list}) do
    decimal_len = Enum.count(decimal_list)
    numerator = Ratio.pow(10, decimal_len)
    integer = List.to_integer(integer_list)
    decimal = List.to_integer(decimal_list)

    (integer * numerator + decimal) <|> numerator
  end
end
