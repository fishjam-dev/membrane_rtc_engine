defmodule Coerce do
  @moduledoc """
  Coerce allows defining coercions between data types.

  These are standardized conversions of one kind of data to another.
  A coercion can be defined using `defcoercion`.

  The code that coercion is compiled to attempts to ensure that the result
  is relatively fast (with the possibility for further optimization in the future).

  Coerce does _not_ come with built-in coercions, instead allowing libraries that build on top of it
  to define their own rules.
  """

  @builtin_guards_list [
    is_tuple: Tuple,
    is_atom: Atom,
    is_list: List,
    is_map: Map,
    is_bitstring: BitString,
    is_integer: Integer,
    is_float: Float,
    is_function: Function,
    is_pid: PID,
    is_port: Port,
    is_reference: Reference]

  @doc """
  Performs value coercion,

  the simpler of the two values is converted into
  a more complex type, and the result is returned as tuple.


  ## Examples

      iex> require Coerce
      iex> Coerce.defcoercion(Integer, Float) do
      iex>   def coerce(int, float) do
      iex>     {int + 0.0, float}
      iex>   end
      iex> end
      iex> Coerce.coerce(1, 2.3)
      {1.0, 2.3}
      iex> Coerce.coerce(1.4, 42)
      {1.4, 42.0}


      iex> require Coerce
      iex> Coerce.defcoercion(BitString, Atom) do
      iex>   def coerce(str, atom) do
      iex>     {str, inspect(atom)}
      iex>   end
      iex> end
      iex> Coerce.coerce("foo", Bar)
      {"foo", "Bar"}
      iex> Coerce.coerce("baz", :qux)
      {"baz", ":qux"}

  """
  @spec coerce(a, b) :: {a, a} | {b, b} when a: any, b: any
  def coerce(a, b)

  def coerce(a = %a_mod{}, b = %a_mod{}) do
    {a, b}
  end
  def coerce(a = %a_mod{}, b = %b_mod{}) do
    Module.concat([Coerce.Implementations, a_mod, b_mod]).coerce(a, b)
  end

  for {guard, mod} <- @builtin_guards_list do
    def coerce(a = %a_struct_mod{}, b) when unquote(guard)(b) do
      Module.concat([Coerce.Implementations, a_struct_mod, unquote(mod)]).coerce(a, b)
    end

    def coerce(a, b = %b_struct_mod{}) when unquote(guard)(a) do
      Module.concat([Coerce.Implementations, unquote(mod), b_struct_mod]).coerce(a, b)
    end
  end

  for {guard_a, a_mod} <- @builtin_guards_list, {guard_b, b_mod} <- @builtin_guards_list do
    if guard_a == guard_b do
      def coerce(a, b) when unquote(guard_a)(a) and unquote(guard_a)(b) do
        {a, b}
      end

    else
      primary_module = Module.concat([Coerce.Implementations, a_mod, b_mod])
      def coerce(a, b) when unquote(guard_a)(a) and unquote(guard_b)(b) do
        # Uses Kernel.apply to mitigate warning if module does not exist...
        apply(unquote(primary_module), :coerce, [a, b])
      end
    end
  end

  @doc """
  Define a coercion between two data types.

  Expects two module names as the first two arguments and a `do`-block as third argument.
  A `Coerc.CompileError` will be raised at compile-time if the coercion macro is called improperly.

  """
  defmacro defcoercion(first_module, second_module, [do: block]) do
    first_module = Macro.expand_once(first_module, __CALLER__)
    second_module = Macro.expand_once(second_module, __CALLER__)

    unless is_atom(first_module) && is_atom(second_module) do
      raise Coerce.CompileError, "`Coerce.defcoercion` called with improper arguments. Expects #{inspect(first_module)} and #{inspect(second_module)} to be module names."
    end
    primary_module = Module.concat([Coerce.Implementations, first_module, second_module])
    secondary_module = Module.concat([Coerce.Implementations, second_module, first_module])

    quote do
      defmodule unquote(primary_module) do
        @moduledoc false
        unquote(block)
      end
      unless function_exported?(unquote(primary_module), :coerce, 2) do
        raise Coerce.CompileError, "`Coerce.defcoercion` implementation does not implement `coerce/2`."
      end

      defmodule unquote(secondary_module) do
        @moduledoc false
        def coerce(lhs, rhs) do
          {rhs, lhs} = unquote(primary_module).coerce(rhs, lhs)
          {lhs, rhs}
        end
      end
    end
  end
end
