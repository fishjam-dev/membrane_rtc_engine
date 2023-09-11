defmodule Bunch do
  @moduledoc """
  A bunch of general-purpose helper and convenience functions.
  """

  alias __MODULE__.Type

  @doc """
  Imports a bunch of Bunch macros: `withl/1`, `withl/2`, `~>/2`, `~>>/2`, `quote_expr/1`, `quote_expr/2`
  """
  defmacro __using__(_args) do
    quote do
      import unquote(__MODULE__),
        only: [
          withl: 1,
          withl: 2,
          ~>: 2,
          ~>>: 2,
          quote_expr: 1,
          quote_expr: 2,
          then_if: 3
        ]
    end
  end

  @compile {:inline, listify: 1, error_if_nil: 2}

  @doc """
  Extracts the key from a key-value tuple.
  """
  @spec key({key, value}) :: key when key: any, value: any
  def key({key, _value}), do: key

  @doc """
  Extracts the value from a key-value tuple.
  """
  @spec value({key, value}) :: value when key: any, value: any
  def value({_key, value}), do: value

  @doc """
  Creates a short reference.
  """
  @spec make_short_ref() :: Bunch.ShortRef.t()
  defdelegate make_short_ref, to: Bunch.ShortRef, as: :new

  @doc """
  Works like `quote/2`, but doesn't require a do/end block and options are passed
  as the last argument.

  Useful when quoting a single expression.

  ## Examples

      iex> use Bunch
      iex> quote_expr(String.t())
      quote do String.t() end
      iex> quote_expr(unquote(x) + 2, unquote: false)
      quote unquote: false do unquote(x) + 2 end

  ## Nesting
  Nesting calls to `quote` disables unquoting in the inner call, while placing
  `quote_expr` in `quote` or another `quote_expr` does not:

      iex> use Bunch
      iex> quote do quote do unquote(:code) end end == quote do quote do :code end end
      false
      iex> quote do quote_expr(unquote(:code)) end == quote do quote_expr(:code) end
      true

  """
  defmacro quote_expr(code, opts \\ []) do
    {:quote, [], [opts, [do: code]]}
  end

  @doc """
  A labeled version of the `with/1` macro.

  This macro works like `with/1`, but enforces user to mark corresponding `withl`
  and `else` clauses with the same label (atom). If a `withl` clause does not
  match, only the `else` clauses marked with the same label are matched against
  the result.


      iex> use #{inspect(__MODULE__)}
      iex> names = %{1 => "Harold", 2 => "Małgorzata"}
      iex> test = fn id ->
      ...>   withl id: {int_id, _} <- Integer.parse(id),
      ...>         name: {:ok, name} <- Map.fetch(names, int_id) do
      ...>     {:ok, "The name is \#{name}"}
      ...>   else
      ...>     id: :error -> {:error, :invalid_id}
      ...>     name: :error -> {:error, :name_not_found}
      ...>   end
      ...> end
      iex> test.("1")
      {:ok, "The name is Harold"}
      iex> test.("5")
      {:error, :name_not_found}
      iex> test.("something")
      {:error, :invalid_id}


  `withl` clauses using no `<-` operator are supported, but they also have to be
  labeled due to Elixir syntax restrictions.


      iex> use #{inspect(__MODULE__)}
      iex> names = %{1 => "Harold", 2 => "Małgorzata"}
      iex> test = fn id ->
      ...>   withl id: {int_id, _} <- Integer.parse(id),
      ...>         do: int_id = int_id + 1,
      ...>         name: {:ok, name} <- Map.fetch(names, int_id) do
      ...>     {:ok, "The name is \#{name}"}
      ...>   else
      ...>     id: :error -> {:error, :invalid_id}
      ...>     name: :error -> {:error, :name_not_found}
      ...>   end
      ...> end
      iex> test.("0")
      {:ok, "The name is Harold"}


  All the `withl` clauses that use `<-` operator must have at least one corresponding
  `else` clause.


      iex> use #{inspect(__MODULE__)}
      iex> try do
      ...>   Code.compile_quoted(quote do
      ...>     withl a: a when a > 0 <- 1,
      ...>           b: b when b > 0 <- 2 do
      ...>       {:ok, a + b}
      ...>     else
      ...>       a: _ -> :error
      ...>     end
      ...>   end)
      ...> rescue
      ...>   e -> e.description
      ...> end
      "Label :b not present in withl else clauses"


  ## Variable scoping

  Because the labels are resolved in the compile time, they make it possible to
  access results of already succeeded matches from `else` clauses. This may help
  handling errors, like below:


      iex> use #{inspect(__MODULE__)}
      iex> names = %{1 => "Harold", 2 => "Małgorzata"}
      iex> test = fn id ->
      ...>   withl id: {int_id, _} <- Integer.parse(id),
      ...>         do: int_id = int_id + 1,
      ...>         name: {:ok, name} <- Map.fetch(names, int_id) do
      ...>     {:ok, "The name is \#{name}"}
      ...>   else
      ...>     id: :error -> {:error, :invalid_id}
      ...>     name: :error -> {:ok, "The name is Defaultius the \#{int_id}th"}
      ...>   end
      ...> end
      iex> test.("0")
      {:ok, "The name is Harold"}
      iex> test.("5")
      {:ok, "The name is Defaultius the 6th"}


  ## Duplicate labels

  `withl` supports marking multiple `withl` clauses with the same label, however
  in that case all the `else` clauses marked with such label are simply put multiple
  times into the generated code. Note that this may lead to confusion, in particular
  when variables are rebound in `withl` clauses:


      iex> use #{inspect(__MODULE__)}
      iex> test = fn x ->
      ...>   withl a: x when x > 1 <- x,
      ...>         do: x = x + 1,
      ...>         a: x when x < 4 <- x do
      ...>     :ok
      ...>   else
      ...>     a: x -> {:error, x}
      ...>   end
      ...> end
      iex> test.(2)
      :ok
      iex> test.(1)
      {:error, 1}
      iex> test.(3)
      {:error, 4}


  """
  @spec withl(keyword(with_clause :: term), do: code_block :: term(), else: match_clauses :: term) ::
          term
  defmacro withl(with_clauses, do: block, else: else_clauses)
           when is_list(with_clauses) and is_list(else_clauses) do
    do_withl(with_clauses, block, else_clauses, __CALLER__)
  end

  @doc """
  Works like `withl/2`, but allows shorter syntax.

  ## Examples

      iex> use #{inspect(__MODULE__)}
      iex> x = 1
      iex> y = 2
      iex> withl a: true <- x > 0,
      ...>       b: false <- y |> rem(2) == 0,
      ...>       do: {x, y},
      ...>       else: (a: false -> {:error, :x}; b: true -> {:error, :y})
      {:error, :y}


  For more details and more verbose and readable syntax, check docs for `withl/2`.
  """
  @spec withl(
          keyword :: [
            {key :: atom(), with_clause :: term}
            | {:do, code_block :: term}
            | {:else, match_clauses :: term}
          ]
        ) :: term
  defmacro withl(keyword) when is_list(keyword) do
    {{:else, else_clauses}, keyword} = keyword |> List.pop_at(-1)
    {{:do, block}, keyword} = keyword |> List.pop_at(-1)
    with_clauses = keyword
    do_withl(with_clauses, block, else_clauses, __CALLER__)
  end

  defp do_withl(with_clauses, block, else_clauses, caller) do
    else_clauses =
      else_clauses
      |> Enum.map(fn {:->, meta, [[[{label, left}]], right]} ->
        {label, {:->, meta, [[left], right]}}
      end)
      |> Enum.group_by(fn {k, _v} -> k end, fn {_k, v} -> v end)

    {result, used_labels} =
      with_clauses
      |> Enum.reverse()
      |> Enum.reduce({block, []}, fn
        {label, {:<-, meta, _args} = clause}, {acc, used_labels} ->
          label_else_clauses =
            else_clauses[label] ||
              "Label #{inspect(label)} not present in withl else clauses"
              |> raise_compile_error(caller, meta)

          {{:with, meta, [clause, [do: acc, else: label_else_clauses]]}, [label | used_labels]}

        {_label, clause}, {acc, used_labels} ->
          {quote do
             unquote(clause)
             unquote(acc)
           end, used_labels}
      end)

    unused_else_clauses = Map.drop(else_clauses, used_labels)

    Enum.each(unused_else_clauses, fn {label, clauses} ->
      Enum.each(clauses, fn {:->, meta, _args} ->
        log_compile_warning(
          "withl's else clause labelled #{inspect(label)} will never match",
          caller,
          meta
        )
      end)
    end)

    result
  end

  @doc """
  Embeds the argument in a one-element list if it is not a list itself. Otherwise
  works as identity.

  Works similarly to `List.wrap/1`, but treats `nil` as any non-list value,
  instead of returning empty list in this case.

  ## Examples

      iex> #{inspect(__MODULE__)}.listify(:a)
      [:a]
      iex> #{inspect(__MODULE__)}.listify([:a, :b, :c])
      [:a, :b, :c]
      iex> #{inspect(__MODULE__)}.listify(nil)
      [nil]

  """
  @spec listify(a | [a]) :: [a] when a: any
  def listify(list) when is_list(list) do
    list
  end

  def listify(non_list) do
    [non_list]
  end

  @doc """
  Returns an `:error` tuple if given value is `nil` and `:ok` tuple otherwise.

  ## Examples

      iex> map = %{:answer => 42}
      iex> #{inspect(__MODULE__)}.error_if_nil(map[:answer], :reason)
      {:ok, 42}
      iex> #{inspect(__MODULE__)}.error_if_nil(map[:invalid], :reason)
      {:error, :reason}

  """
  @spec error_if_nil(value, reason) :: Type.try_t(value)
        when value: any(), reason: any()
  def error_if_nil(nil, reason), do: {:error, reason}
  def error_if_nil(v, _reason), do: {:ok, v}

  @doc """
  Returns given stateful try value along with its status.
  """
  @spec stateful_try_with_status(result) :: {status, result}
        when status: Type.try_t(),
             result:
               Type.stateful_try_t(state :: any) | Type.stateful_try_t(value :: any, state :: any)
  def stateful_try_with_status({:ok, _state} = res), do: {:ok, res}
  def stateful_try_with_status({{:ok, _res}, _state} = res), do: {:ok, res}
  def stateful_try_with_status({{:error, reason}, _state} = res), do: {{:error, reason}, res}

  @doc """
  Helper for writing pipeline-like syntax. Maps given value using match clauses
  or lambda-like syntax.

  ## Examples

      iex> use #{inspect(__MODULE__)}
      iex> {:ok, 10} ~> ({:ok, x} -> x)
      10
      iex> 5 ~> &1 + 2
      7

  Lambda-like expressions are not converted to lambdas under the hood, but
  result of `expr` is injected to `&1` at the compile time.

  Useful especially when dealing with a pipeline of operations (made up e.g.
  with pipe (`|>`) operator) some of which are hard to express in such form:

      iex> use #{inspect(__MODULE__)}
      iex> ["Joe", "truck", "jacket"]
      ...> |> Enum.map(&String.downcase/1)
      ...> |> Enum.filter(& &1 |> String.starts_with?("j"))
      ...> ~> ["Words:" | &1]
      ...> |> Enum.join("\\n")
      "Words:
      joe
      jacket"

  """
  # Case when the mapper is a list of match clauses
  defmacro expr ~> ([{:->, _, _} | _] = mapper) do
    quote do
      case unquote(expr) do
        unquote(mapper)
      end
    end
  end

  # Case when the mapper is a piece of lambda-like code
  defmacro expr ~> mapper do
    {mapped, arg_present?} =
      mapper
      |> Macro.prewalk(false, fn
        {:&, _meta, [1]}, _acc ->
          quote do: {expr_result, true}

        {:&, _meta, [i]} = node, acc when is_integer(i) ->
          {node, acc}

        {:&, meta, _args}, _acc ->
          """
          The `&` (capture) operator is not allowed in lambda-like version of \
          `#{inspect(__MODULE__)}.~>/2`. Use `&1` alone instead.
          """
          |> raise_compile_error(__CALLER__, meta)

        other, acc ->
          {other, acc}
      end)

    if not arg_present? do
      """
      `#{inspect(__MODULE__)}.~>/2` operator requires either match clauses or \
      at least one occurrence of `&1` argument on the right hand side.
      """
      |> raise_compile_error(__CALLER__)
    end

    quote do
      expr_result = unquote(expr)
      unquote(mapped)
    end
  end

  @doc """
  Works similar to `~>/2`, but accepts only `->` clauses and appends default
  identity clause at the end.

  ## Examples

      iex> use #{inspect(__MODULE__)}
      iex> {:ok, 10} ~>> ({:ok, x} -> {:ok, x+1})
      {:ok, 11}
      iex> :error ~>> ({:ok, x} -> {:ok, x+1})
      :error

  """
  defmacro expr ~>> ([{:->, _, _} | _] = mapper_clauses) do
    default =
      quote do
        default_result -> default_result
      end

    quote do
      case unquote(expr) do
        unquote(mapper_clauses ++ default)
      end
    end
  end

  defmacro _expr ~>> _ do
    """
    `#{inspect(__MODULE__)}.~>>/2` operator expects match clauses on the right \
    hand side.
    """
    |> raise_compile_error(__CALLER__)
  end

  @spec raise_compile_error(term(), Macro.Env.t(), Keyword.t()) :: no_return()
  @spec raise_compile_error(term(), Macro.Env.t()) :: no_return()
  defp raise_compile_error(reason, caller, meta \\ []) do
    raise CompileError,
      file: caller.file,
      line: meta |> Keyword.get(:line, caller.line),
      description: reason
  end

  defp log_compile_warning(warning, caller, meta) do
    stacktrace =
      caller |> Map.update!(:line, &Keyword.get(meta, :line, &1)) |> Macro.Env.stacktrace()

    IO.warn(warning, stacktrace)
  end

  @doc """
  Maps a value `x` with a `function` if the condition is true, acts as
  an identity function otherwise.

  ## Examples
    iex> use #{inspect(__MODULE__)}
    iex> then_if(1, false, & &1 + 1)
    1
    iex> then_if(1, true, & &1 + 1)
    2
    iex> arg = 1
    iex> arg |> then_if(not is_list(arg), fn arg -> [arg] end) |> Enum.map(&(&1*2))
    [2]
  """
  @spec then_if(x, condition :: boolean(), f :: (x -> y)) :: y when x: any(), y: any()
  def then_if(x, condition, f) do
    if condition do
      f.(x)
    else
      x
    end
  end
end
