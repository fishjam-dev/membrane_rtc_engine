defmodule Membrane.RTC.Utils do
  @moduledoc false

  alias Membrane.Core.Pipeline.CallbackContext

  @opaque callback_context_t() :: CallbackContext.t()

  @spec find_child(ctx :: any(), pattern: pattern :: any()) :: Macro.t()
  defmacro find_child(ctx, pattern: pattern) do
    quote do
      unquote(ctx).children |> Map.keys() |> Enum.find(&match?(unquote(pattern), &1))
    end
  end

  @spec reduce_children(ctx :: callback_context_t(), acc :: any(), fun :: fun()) ::
          any()
  def reduce_children(ctx, acc, fun) do
    ctx.children |> Enum.reduce(acc, fun)
  end

  @spec flat_map_children(ctx :: callback_context_t(), fun :: fun()) :: [any()]
  def flat_map_children(ctx, fun) do
    ctx.children |> Enum.flat_map(fun)
  end
end
