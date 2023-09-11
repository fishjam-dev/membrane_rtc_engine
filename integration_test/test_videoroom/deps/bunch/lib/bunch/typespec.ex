defmodule Bunch.Typespec do
  @moduledoc """
  A bunch of typespec-related helpers.
  """

  @deprecated "`use Bunch.Typespec` only brings to scope the deprecated @/1"
  defmacro __using__(_args) do
    quote do
      import Kernel, except: [@: 1]
      import unquote(__MODULE__), only: [@: 1]
    end
  end

  @doc """
  **This macro is deprecated. Use `#{inspect(__MODULE__)}.enum_to_alternative/1` instead.**

  Allows to define a type in form of `t :: x | y | z | ...` and a module parameter
  in form of `@t [x, y, z, ...]` at once.

  ## Example

      defmodule Abc do
        use #{inspect(__MODULE__)}
        @list_type t :: [:a, :b, :c]
        @spec get_at(0..2) :: t
        def get_at(x), do: @t |> Enum.at(x)
      end

      Abc.get_at(1) # -> :b

  """
  defmacro @{:list_type, _meta1, [{:"::", _meta2, [{name, _meta3, _env} = name_var, list]}]} do
    IO.warn(
      "Bunch.Typespec.@list_type is deprecated. Use #{inspect(__MODULE__)}.enum_to_alternative/1 instead."
    )

    type =
      quote do
        Enum.reduce(unquote(list), fn a, b -> {:|, [], [a, b]} end)
      end

    type = {:unquote, [], [type]}

    quote do
      @type unquote(name_var) :: unquote(type)
      Module.put_attribute(__MODULE__, unquote(name), unquote(list))
    end
  end

  defmacro @expr do
    quote do
      Kernel.@(unquote(expr))
    end
  end

  @doc """
  Converts an enumerable of terms to AST of alternative type of these terms.

  Useful for defining a type out of a list of constants.

  ## Examples

      iex> defmodule Example do
      ...>   @values [1, :a, {true, 3 * 4}]
      ...>   @type value :: unquote(#{inspect(__MODULE__)}.enum_to_alternative(@values))
      ...>   @spec get_value(0..2) :: value
      ...>   def get_value(i), do: Enum.at(@values, i)
      ...> end
      iex> Example.get_value(1)
      :a

      iex> #{inspect(__MODULE__)}.enum_to_alternative([1, :a, {true, 3 * 4}])
      quote do
        1 | :a | {true, 12}
      end

  """
  @spec enum_to_alternative(Enumerable.t()) :: Macro.t()
  def enum_to_alternative(list) do
    list |> Enum.reverse() |> Enum.reduce(fn a, b -> quote do: unquote(a) | unquote(b) end)
  end
end
