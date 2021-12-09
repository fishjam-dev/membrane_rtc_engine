defmodule Membrane.RTC.Utils do
  @moduledoc false

  @spec find_child(ctx :: any(), pattern :: any()) :: Membrane.ChildEntry.t()
  defmacro find_child(ctx, pattern: pattern) do
    quote do
      unquote(ctx).children |> Map.keys() |> Enum.find(&match?(unquote(pattern), &1))
    end
  end

  @spec reduce_children(ctx :: Membrane.Pipeline.CallbackContext.t(), acc :: any(), fun :: fun()) ::
          any()
  def reduce_children(ctx, acc, fun) do
    ctx.children |> Map.keys() |> Enum.reduce(acc, fun)
  end

  @spec flat_map_children(ctx :: Membrane.Pipeline.CallbackContext.t(), fun :: fun()) :: [any()]
  def flat_map_children(ctx, fun) do
    ctx.children |> Map.keys() |> Enum.flat_map(fun)
  end

  @spec forward_msg_to_child(
          child_name :: any(),
          msg :: any(),
          ctx :: Membrane.Pipeline.CallbackContext.t()
        ) :: [Membrane.Element.Action.notify_t()]
  def forward_msg_to_child(child_name, msg, ctx) do
    child = find_child(ctx, pattern: ^child_name)

    if child do
      [forward: {child_name, msg}]
    else
      []
    end
  end
end
