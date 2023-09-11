defimpl Numbers.Protocols.Addition, for: Integer do
  def add(a, b), do: a + b
  def add_id(_), do: 0
end

defimpl Numbers.Protocols.Subtraction, for: Integer do
  def sub(a, b), do: a - b
end

defimpl Numbers.Protocols.Multiplication, for: Integer do
  def mult(a, b), do: a * b
  def mult_id(_), do: 1
end

defimpl Numbers.Protocols.Division, for: Integer do
  def div(a, b), do: a / b
end

defimpl Numbers.Protocols.Minus, for: Integer do
  def minus(num), do: -num
end

defimpl Numbers.Protocols.Absolute, for: Integer do
  def abs(num) when num < 0, do: -num
  def abs(num), do: num
end

defimpl Numbers.Protocols.Exponentiation, for: Integer do
  def pow(x, n) do
    Numbers.Helper.pow_by_sq(x, n)
  end
end
defimpl Numbers.Protocols.ToFloat, for: Integer do
  def to_float(x), do: x + 0.0
end
