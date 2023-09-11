defmodule Bunch.Macro do
  @moduledoc """
  A bunch of helpers for implementing macros.
  """

  @doc """
  Imitates `import` functionality by finding and replacing bare function
  calls (like `foo()`) in AST with fully-qualified call (like `Some.Module.foo()`)

  Receives AST fragment as first parameter and
  list of pairs {Some.Module, :foo} as second
  """
  @spec inject_calls(Macro.t(), [{module(), atom()}]) :: Macro.t()
  def inject_calls(ast, functions)
      when is_list(functions) do
    Macro.prewalk(ast, fn ast_node ->
      functions |> Enum.reduce(ast_node, &replace_call(&2, &1))
    end)
  end

  @doc """
  Imitates `import` functionality by finding and replacing bare function
  calls (like `foo()`) in AST with fully-qualified call (like `Some.Module.foo()`)

  Receives AST fragment as first parameter and
  a pair {Some.Module, :foo} as second
  """
  @spec inject_call(Macro.t(), {module(), atom()}) :: Macro.t()
  def inject_call(ast, {module, fun_name})
      when is_atom(module) and is_atom(fun_name) do
    Macro.prewalk(ast, fn ast_node ->
      replace_call(ast_node, {module, fun_name})
    end)
  end

  defp replace_call(ast_node, {module, fun_name})
       when is_atom(module) and is_atom(fun_name) do
    case ast_node do
      {^fun_name, _ctx, args} ->
        quote do
          apply(unquote(module), unquote(fun_name), unquote(args))
        end

      other_node ->
        other_node
    end
  end

  @doc """
  Works like `Macro.prewalk/2`, but allows to skip particular nodes.

  ## Example

      iex> code = quote do fun(1, 2, opts: [key: :val]) end
      iex> code |> Bunch.Macro.prewalk_while(fn node ->
      ...>   if Keyword.keyword?(node) do
      ...>     {:skip, node ++ [default: 1]}
      ...>   else
      ...>     {:enter, node}
      ...>   end
      ...> end)
      quote do fun(1, 2, opts: [key: :val], default: 1) end

  """
  @spec prewalk_while(Macro.t(), (Macro.t() -> {:enter | :skip, Macro.t()})) :: Macro.t()
  def prewalk_while(ast, fun) do
    {ast, :not_skipping} =
      Macro.traverse(
        ast,
        :not_skipping,
        fn node, :not_skipping ->
          case fun.(node) do
            {:enter, node} -> {node, :not_skipping}
            {:skip, node} -> {nil, {:skipping, node}}
          end
        end,
        fn
          nil, {:skipping, node} -> {node, :not_skipping}
          node, :not_skipping -> {node, :not_skipping}
        end
      )

    ast
  end

  @doc """
  Works like `Macro.prewalk/3`, but allows to skip particular nodes using an accumulator.

  ## Example

      iex> code = quote do fun(1, 2, opts: [key: :val]) end
      iex> code |> Bunch.Macro.prewalk_while(0, fn node, acc ->
      ...>   if Keyword.keyword?(node) do
      ...>     {:skip, node ++ [default: 1], acc + 1}
      ...>   else
      ...>     {:enter, node, acc}
      ...>   end
      ...> end)
      {quote do fun(1, 2, opts: [key: :val], default: 1) end, 1}

  """
  @spec prewalk_while(
          Macro.t(),
          any(),
          (Macro.t(), any() -> {:enter | :skip, Macro.t(), any()})
        ) :: {Macro.t(), any()}
  def prewalk_while(ast, acc, fun) do
    {ast, {acc, :not_skipping}} =
      Macro.traverse(
        ast,
        {acc, :not_skipping},
        fn node, {acc, :not_skipping} ->
          case fun.(node, acc) do
            {:enter, node, acc} -> {node, {acc, :not_skipping}}
            {:skip, node, acc} -> {nil, {acc, {:skipping, node}}}
          end
        end,
        fn
          nil, {acc, {:skipping, node}} -> {node, {acc, :not_skipping}}
          node, {acc, :not_skipping} -> {node, {acc, :not_skipping}}
        end
      )

    {ast, acc}
  end

  @doc """
  Receives an AST and traverses it expanding all the nodes.

  This function uses `Macro.expand/2` under the hood. Check
  it out for more information and examples.
  """
  @spec expand_deep(Macro.t(), Macro.Env.t()) :: Macro.t()
  def expand_deep(ast, env), do: Macro.prewalk(ast, fn tree -> Macro.expand(tree, env) end)
end
