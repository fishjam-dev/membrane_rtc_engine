# Implementations for the :decimal library.
# Implementation exists inside Numbers
# because it is the newer and less mature library of the two.

if Code.ensure_loaded?(Decimal) do
  defimpl Numbers.Protocols.Addition, for: Decimal do
    defdelegate add(a, b), to: Decimal
    def add_id(_num), do: Decimal.new(0)
  end

  defimpl Numbers.Protocols.Subtraction, for: Decimal do
    defdelegate sub(a, b), to: Decimal
  end

  defimpl Numbers.Protocols.Multiplication, for: Decimal do
    defdelegate mult(a, b), to: Decimal
    def mult_id(_num), do: Decimal.new(1)
  end

  defimpl Numbers.Protocols.Division, for: Decimal do
    defdelegate div(a, b), to: Decimal
  end

  defimpl Numbers.Protocols.Minus, for: Decimal do
    defdelegate minus(num), to: Decimal, as: :negate
  end

  defimpl Numbers.Protocols.Absolute, for: Decimal do
    defdelegate abs(num), to: Decimal
  end

  defimpl Numbers.Protocols.ToFloat, for: Decimal do
    defdelegate to_float(num), to: Decimal
  end

  defimpl Numbers.Protocols.Exponentiation, for: Decimal do
    defdelegate pow(num, power), to: Numbers.Helper, as: :pow_by_sq
  end

  require Coerce

  Coerce.defcoercion(Decimal, Integer) do
    def coerce(decimal, integer) do
      {decimal, Decimal.new(integer)}
    end
  end

  Coerce.defcoercion(Decimal, Float) do
    def coerce(decimal, float) do
      {decimal, Decimal.new(float)}
    end
  end
end
