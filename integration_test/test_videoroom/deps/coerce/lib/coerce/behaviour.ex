defmodule Coerce.Behaviour do
  @callback coerce(a, b) :: {a, a} when a: any, b: any
end
