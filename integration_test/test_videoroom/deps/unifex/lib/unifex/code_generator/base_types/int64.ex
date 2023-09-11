defmodule Unifex.CodeGenerator.BaseTypes.Int64 do
  @moduledoc """
  Module implementing `Unifex.CodeGenerator.BaseType` behaviour for 64-bit integer.

  Maps `int64` Unifex type to an `int64_t` native type.

  Implemented only for NIF as function parameter as well as return type.
  """
  use Unifex.CodeGenerator.BaseType

  @impl true
  def generate_native_type(_ctx) do
    ~g<int64_t>
  end
end
