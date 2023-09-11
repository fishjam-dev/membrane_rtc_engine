defmodule Numbers.Operators do
  # Do not import this module directly.
  # Instead, use `use Numbers, override_operators: true`
  # which will import this module's functions properly.
  @moduledoc false

  # The following macro-based implementation
  # ensures that within guard contexts,
  # we'll continue to use the builtin Kernel versions
  # that are allowed to be used in guards.

  # Unary operators
  for {fun, operator} <- [minus: :-, abs: :abs] do
    @fun fun
    @operator operator
    defmacro unquote(operator)(a) do
      case __CALLER__.context do
        :guard ->
          quote do
            Kernel.unquote(@operator)(unquote(a))
          end
        nil ->
          quote do
            Numbers.unquote(@fun)(unquote(a))
          end
      end
    end
  end

  # Binary operators
  for {fun, operator} <- [add: :+, sub: :-, mult: :*, div: :/] do
    @fun fun
    @operator operator
    defmacro unquote(operator)(a, b) do
      case __CALLER__.context do
        context when context in [:guard, :match] ->
          quote do
            Kernel.unquote(@operator)(unquote(a), unquote(b))
          end
        nil ->
          quote do
            Numbers.unquote(@fun)(unquote(a), unquote(b))
          end
      end
    end
  end

  # Operator without conflict with (guard-safe) Kernel function.
  defdelegate pow(a, b), to: Numbers
end
