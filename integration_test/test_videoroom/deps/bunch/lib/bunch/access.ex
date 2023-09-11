defmodule Bunch.Access do
  @moduledoc """
  A bunch of functions for easier manipulation on terms of types implementing `Access`
  behaviour.
  """

  use Bunch

  import Kernel, except: [get_in: 2, put_in: 2, update_in: 3, get_and_update_in: 3, pop_in: 2]

  @compile {:inline, map_keys: 1}

  @gen_common_docs fn fun_name ->
    """
    Works like `Kernel.#{fun_name}` with small differences.

    Behaviour differs in the following aspects:
    - empty lists of keys are allowed
    - single key does not have to be wrapped in a list
    """
  end

  @doc """
  Implements `Access` behaviour by delegating callbacks to `Map` module.

  All the callbacks are overridable.
  """
  defmacro __using__(_args) do
    quote do
      @behaviour Access

      @impl true
      defdelegate fetch(term, key), to: Map

      @impl true
      defdelegate get_and_update(data, key, list), to: Map

      @impl true
      defdelegate pop(data, key), to: Map

      defoverridable Access
    end
  end

  @doc """
  #{@gen_common_docs.("get_in/2")}

  ## Examples

      iex> #{inspect(__MODULE__)}.get_in(%{a: %{b: 10}}, [:a, :b])
      10
      iex> #{inspect(__MODULE__)}.get_in(%{a: 10}, :a)
      10
      iex> #{inspect(__MODULE__)}.get_in(%{a: %{b: 10}}, [])
      %{a: %{b: 10}}

  """
  @spec get_in(Access.t(), Access.key() | [Access.key()]) :: Access.value()
  def get_in(container, []), do: container
  def get_in(container, keys), do: container |> Kernel.get_in(keys |> map_keys)

  @doc """
  #{@gen_common_docs.("put_in/3")}

  ## Examples

      iex> #{inspect(__MODULE__)}.put_in(%{a: %{b: 10}}, [:a, :b], 20)
      %{a: %{b: 20}}
      iex> #{inspect(__MODULE__)}.put_in(%{a: 10}, :a, 20)
      %{a: 20}
      iex> #{inspect(__MODULE__)}.put_in(%{a: %{b: 10}}, [], 20)
      20

  """
  @spec put_in(Access.t(), Access.key() | [Access.key()], Access.value()) :: Access.value()
  def put_in(_map, [], v), do: v
  def put_in(container, keys, v), do: container |> Kernel.put_in(keys |> map_keys, v)

  @doc """
  #{@gen_common_docs.("update_in/3")}

  ## Examples

      iex> #{inspect(__MODULE__)}.update_in(%{a: %{b: 10}}, [:a, :b], & &1 * 2)
      %{a: %{b: 20}}
      iex> #{inspect(__MODULE__)}.update_in(%{a: 10}, :a, & &1 * 2)
      %{a: 20}
      iex> #{inspect(__MODULE__)}.update_in(10, [], & &1 * 2)
      20

  """
  @spec update_in(Access.t(), Access.key() | [Access.key()], (Access.value() -> Access.value())) ::
          Access.t()
  def update_in(container, [], f), do: f.(container)
  def update_in(container, keys, f), do: container |> Kernel.update_in(keys |> map_keys, f)

  @doc """
  #{@gen_common_docs.("get_and_update_in/3")}

  ## Examples

      iex> #{inspect(__MODULE__)}.get_and_update_in(%{a: %{b: 10}}, [:a, :b], & {&1, &1 * 2})
      {10, %{a: %{b: 20}}}
      iex> #{inspect(__MODULE__)}.get_and_update_in(%{a: 10}, :a, & {&1, &1 * 2})
      {10, %{a: 20}}
      iex> #{inspect(__MODULE__)}.get_and_update_in(10, [], & {&1, &1 * 2})
      {10, 20}

  """
  @spec get_and_update_in(Access.t(), Access.key() | [Access.key()], (a -> {b, a})) ::
          {b, Access.t()}
        when a: Access.value(), b: any
  def get_and_update_in(container, [], f), do: f.(container)

  def get_and_update_in(container, keys, f),
    do: container |> Kernel.get_and_update_in(keys |> map_keys, f)

  @doc """
  Updates value at `keys` in a nested data structure and returns new value and updated structure.

  Uses `get_and_update_in/3` under the hood.

  ## Example

      iex> %{a: %{b: 10}} |> #{inspect(__MODULE__)}.get_updated_in([:a, :b], & &1+1)
      {11, %{a: %{b: 11}}}

  """
  @spec get_updated_in(Access.t(), Access.key() | [Access.key()], (Access.value() -> a)) ::
          {a, Access.t()}
        when a: Access.value()
  def get_updated_in(container, keys, f),
    do: container |> get_and_update_in(keys, fn a -> f.(a) ~> {&1, &1} end)

  @doc """
  #{@gen_common_docs.("pop_in/2")}

  ## Examples

      iex> #{inspect(__MODULE__)}.pop_in(%{a: %{b: 10}}, [:a, :b])
      {10, %{a: %{}}}
      iex> #{inspect(__MODULE__)}.pop_in(%{a: 10}, :a)
      {10, %{}}
      iex> #{inspect(__MODULE__)}.pop_in(10, [])
      {10, nil}

  """
  @spec pop_in(Access.t(), Access.key() | [Access.key()]) :: {Access.value(), Access.t()}
  def pop_in(container, []), do: {container, nil}
  def pop_in(container, keys), do: container |> Kernel.pop_in(keys |> map_keys)

  @doc """
  Works like `pop_in/2`, but discards returned value.

  ## Examples

      iex> #{inspect(__MODULE__)}.delete_in(%{a: %{b: 10}}, [:a, :b])
      %{a: %{}}
      iex> #{inspect(__MODULE__)}.delete_in(%{a: 10}, :a)
      %{}
      iex> #{inspect(__MODULE__)}.delete_in(10, [])
      nil

  """
  @spec delete_in(Access.t(), Access.key() | [Access.key()]) :: Access.t()
  def delete_in(container, keys), do: pop_in(container, keys) ~> ({_out, container} -> container)

  @spec map_keys(Access.key() | [Access.key()]) :: [Access.key()]
  defp map_keys(keys), do: keys |> Bunch.listify()
end
