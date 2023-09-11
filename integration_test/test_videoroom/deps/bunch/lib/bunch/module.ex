defmodule Bunch.Module do
  @moduledoc """
  A bunch of functions for easier manipulation on modules.
  """

  @doc """
  Determines whether module implements a behaviour by checking a test function.

  Checked behaviour needs to define a callback with unique name and no arguments,
  that should return `true`. This functions ensures that the module is loaded and
  checks if it exports implementation of the callback that returns `true`. If
  all these conditions are met, `true` is returned. Otherwise returns `false`.
  """
  @spec check_behaviour(module, atom) :: boolean
  def check_behaviour(module, fun_name) do
    module |> loaded_and_function_exported?(fun_name, 0) and module |> apply(fun_name, [])
  end

  @doc """
  Returns instance of struct defined in given module, if the module defines struct.
  Otherwise returns `nil`.

  Raises if struct has any required fields.
  """
  @spec struct(module) :: struct | nil
  def struct(module) do
    if module |> loaded_and_function_exported?(:__struct__, 0),
      do: module.__struct__([]),
      else: nil
  end

  @doc """
  Ensures that module is loaded and checks whether it exports given function.
  """
  @spec loaded_and_function_exported?(module, atom, non_neg_integer) :: boolean
  def loaded_and_function_exported?(module, fun_name, arity) do
    module |> Code.ensure_loaded?() and module |> function_exported?(fun_name, arity)
  end

  @doc """
  Works like `Kernel.apply/3` if `module` exports `fun_name/length(args)`,
  otherwise returns `default`.

  Determines if function is exported using `loaded_and_function_exported?/3`.
  """
  @spec apply(module, fun_name :: atom, args :: list, default :: any) :: any
  def apply(module, fun_name, args, default) do
    if module |> loaded_and_function_exported?(fun_name, length(args)) do
      module |> Kernel.apply(fun_name, args)
    else
      default
    end
  end
end
