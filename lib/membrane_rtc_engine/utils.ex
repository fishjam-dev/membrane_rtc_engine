defmodule Membrane.RTC.Utils do
  defmacro find_child(ctx, pattern: pattern) do
    quote do
      unquote(ctx).children |> Map.keys() |> Enum.find(&match?(unquote(pattern), &1))
    end
  end

  def reduce_children(ctx, acc, fun) do
    ctx.children |> Map.keys() |> Enum.reduce(acc, fun)
  end

  def flat_map_children(ctx, fun) do
    ctx.children |> Map.keys() |> Enum.flat_map(fun)
  end
end
