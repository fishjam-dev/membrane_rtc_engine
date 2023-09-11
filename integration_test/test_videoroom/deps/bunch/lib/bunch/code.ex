defmodule Bunch.Code do
  @moduledoc """
  A bunch of helper functions for code compilation, code evaluation, and code loading.
  """

  @doc """
  Takes a code block, expands macros inside and pretty prints it.
  """
  defmacro peek_code(do: block) do
    block
    |> Bunch.Macro.expand_deep(__CALLER__)
    |> Macro.to_string()
    |> IO.puts()

    block
  end

  @doc """
  Returns stacktrace as a string.

  The stacktrace is formatted to the readable format.
  """
  defmacro stacktrace do
    quote do
      {:current_stacktrace, trace} = Process.info(self(), :current_stacktrace)

      # drop excludes `Process.info/2` call
      trace
      |> Enum.drop(1)
      |> Exception.format_stacktrace()
    end
  end
end
