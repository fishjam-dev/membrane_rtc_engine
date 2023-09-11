if Code.ensure_loaded?(Decimal) do
  defmodule Ratio.DecimalConversion do
    def decimal_to_rational(%Decimal{coef: coef, exp: 0, sign: sign}) do
      numerator = coef * sign
      Ratio.new(numerator)
    end

    def decimal_to_rational(%Decimal{coef: coef, exp: exp, sign: sign}) do
      numerator = coef * sign
      denominator = Ratio.pow(10, exp * -1)
      Ratio.new(numerator, denominator)
    end
  end
end
