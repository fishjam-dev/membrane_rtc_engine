# Unary protocols:
defimpl Numbers.Protocols.Minus, for: Ratio do
  def minus(val) do
    Ratio.minus(val)
  end
end

defimpl Numbers.Protocols.Absolute, for: Ratio do
  def abs(val) do
    Ratio.abs(val)
  end
end

defimpl Numbers.Protocols.ToFloat, for: Ratio do
  def to_float(val) do
    Ratio.to_float(val)
  end
end

# Binary protocols:
defimpl Numbers.Protocols.Addition, for: Ratio do
  def add(lhs, rhs) do
    Ratio.add(lhs, rhs)
  end

  def add_id(_) do
    Ratio.new(0)
  end
end

defimpl Numbers.Protocols.Subtraction, for: Ratio do
  def sub(lhs, rhs) do
    Ratio.sub(lhs, rhs)
  end
end

defimpl Numbers.Protocols.Multiplication, for: Ratio do
  def mult(lhs, rhs) do
    Ratio.mult(lhs, rhs)
  end

  def mult_id(_) do
    Ratio.new(1)
  end
end

defimpl Numbers.Protocols.Division, for: Ratio do
  def div(lhs, rhs) do
    Ratio.div(lhs, rhs)
  end
end

defimpl Numbers.Protocols.Exponentiation, for: Ratio do
  def pow(lhs, integer_power) do
    Ratio.pow(lhs, integer_power)
  end
end
