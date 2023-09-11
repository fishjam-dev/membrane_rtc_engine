# Contains all protocols that Numbers dispatches to
# for its different functionality.

defmodule Numbers.Protocols do
  @moduledoc """
  A set of protocols that can be implemented for your data structure, to add `Numbers`-support to it. 

  In older versions of `Numbers`, structures were required to follow a single, very strict, behaviour.
  But because there are many different kind of structures that benefit from a numeric interface, including
  those for which one or multiple of these operations cannot be (unambiguously) defined,
  this has been split in these different protocols.

  By using the different protocols, each data structure can 'pick and choose' what functionality
  is supported. As protocol dispatching is used, the result should be a lot faster than in older
  versions of Numbers, which performed behaviour-based runtime dispatch on the struct name.


  ## Coercion

  Numbers does not automatically transform numbers from one type to another if one of the functions is called with two different types.

  Frequently you do want to use other data types together with your custom data type. For this, a custom coercion can be specified,
  using `Coerce.defcoercion/3` as exposed by the [`Coerce`](https://hex.pm/packages/coerce) library that `Numbers` depends on.

  """
end

defprotocol Numbers.Protocols.Addition do
  @moduledoc """
  For supporting `Numbers.add/2`.
  """

  @doc """
  Adds two numbers together.
  """
  @spec add(t, t) :: t
  def add(a, b)

  @doc """
  Should return the 'additive identity' of the same type as the argument.
  This is the value that can be added to another number,
  to let the result remain equal to that number.

  (For integers, this is `0`, for floats, `0.0`. Most other numeric types have their own 'zero' variant as well.)

  This should be defined so that:

      a = some_num
      add(a, add_id()) == a
      add(add_id(), a) == a

  If the numeric structure also implements `Numbers.Protocols.Subtraction`, the following should also be true:

      a = some_num
      sub(a, add_id()) == a
      sub(add_id(), a) == a

  (Note that it is fine if the result is not structurally identical, as long as it is logically equal.)
  """
  @spec add_id(t) :: t
  def add_id(_num)
end

defprotocol Numbers.Protocols.Subtraction do
  @moduledoc """
  For supporting `Numbers.sub/2`.
  """

  @doc """
  Subtracts the rhs number from the lhs number.
  """
  @spec sub(t, t) :: t
  def sub(a, b)
end

defprotocol Numbers.Protocols.Minus do
  @moduledoc """
  For supporting `Numbers.minus/1`.
  """

  @doc """
  Unary minus. Should return the negation of the number.
  """
  @spec minus(t) :: t
  def minus(num)
end

defprotocol Numbers.Protocols.Absolute do
  @moduledoc """
  For supporting `Numbers.abs/1`.
  """

  @doc """
  The absolute value of a number.
  """
  @spec abs(t) :: t
  def abs(num)
end

defprotocol Numbers.Protocols.Multiplication do
  @moduledoc """
  For supporting `Numbers.mult/2`.
  """

  @doc """
  Multiplies the two numbers together.
  """
  @spec mult(t, t) :: t
  def mult(a, b)

  @doc """
  Should return the 'multiplicative identity' of the same type as the argument.
  This is the value that can be added to another number,
  to let the result remain equal to that number.

  (For integers, this is `1`, for floats, `1.0`. Most other numeric types have their own 'one' variant as well.)

  This should be defined so that:

      a = some_num
      mult(a, mult_id()) == a
      mult(mult_id(), a) == a

  If the numeric structure also implements `Numbers.Protocols.Division`, the following should also be true:

      a = some_num
      div(a, mult_id()) == a
      div(mult_id(), a) == a

  (Note that it is fine if the result is not structurally identical, as long as it is logically equal.)
  """
  @spec mult_id(t) :: t
  def mult_id(_num)
end

defprotocol Numbers.Protocols.Division do
  @moduledoc """
  For supporting `Numbers.div/2`.
  """

  @doc """
  Divides the rhs by the lhs.

  To be clear, this division operation is supposed to be precise.
  """
  @spec div(t, t) :: t
  def div(a, b)
end

defprotocol Numbers.Protocols.Exponentiation do
  @moduledoc """
  For supporting `Numbers.pow/2`.
  """

  @doc """
  Power function, x^n.

  Unless a dedicated fast power algorithm exists for your data structure,
  you could use the 'Exponentiation by Squaring' algorithm, by calling
  `Numbers.Helper.pow_by_sq(num, integer_power)` in the implementation,
  which is a reasonably fast algorithm that uses `log(n)` multiplication steps.
  """
  @spec pow(t, non_neg_integer) :: t
  def pow(num, integer_power)
end

defprotocol Numbers.Protocols.ToFloat do
  @moduledoc """
  For supporting `Numbers.to_float/1`.
  """

  @doc """
  Convert the custom Numeric struct
  to the built-in float datatype.

  It is okay to lose precision during this conversion.
  """
  @spec to_float(t) :: {:ok, t_as_float :: float} | :error
  def to_float(num)
end
