defmodule Bunch.KVEnum do
  @moduledoc """
  A bunch of helper functions for manipulating key-value enums (including keyword
  enums).

  Key-value enums are represented as enums of 2-element tuples, where the first
  element of each tuple is a key, and the second is a value.
  """

  @type t(_key, _value) :: Enumerable.t()

  @doc """
  Returns all keys from the `enum`.

  Duplicated keys appear duplicated in the final enum of keys.

  ## Examples

      iex> #{inspect(__MODULE__)}.keys(a: 1, b: 2)
      [:a, :b]
      iex> #{inspect(__MODULE__)}.keys(a: 1, b: 2, a: 3)
      [:a, :b, :a]

  """
  @spec keys(t(key, value)) :: [key] when key: any, value: any
  def keys(enum) do
    Enum.map(enum, &Bunch.key/1)
  end

  @doc """
  Returns all values from the `enum`.

  Values from duplicated keys will be kept in the final enum of values.

  ## Examples

      iex> #{inspect(__MODULE__)}.values(a: 1, b: 2)
      [1, 2]
      iex> #{inspect(__MODULE__)}.values(a: 1, b: 2, a: 3)
      [1, 2, 3]

  """
  @spec values(t(key, value)) :: [value] when key: any, value: any
  def values(enum) do
    Enum.map(enum, &Bunch.value/1)
  end

  @doc """
  Maps keys of `enum` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_keys([{1, :a}, {2, :b}], & &1+1)
      [{2, :a}, {3, :b}]

  """
  @spec map_keys(t(k1, v), (k1 -> k2)) :: t(k2, v) when k1: any, k2: any, v: any
  def map_keys(enum, f) do
    enum |> Enum.map(fn {key, value} -> {f.(key), value} end)
  end

  @doc """
  Maps values of `enum` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_values([a: 1, b: 2], & &1+1)
      [a: 2, b: 3]

  """
  @spec map_values(t(k, v1), (v1 -> v2)) :: t(k, v2) when k: any, v1: any, v2: any
  def map_values(enum, f) do
    enum |> Enum.map(fn {key, value} -> {key, f.(value)} end)
  end

  @doc """
  Filters elements of `enum` by keys using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.filter_by_keys([a: 1, b: 2, a: 3], & &1 == :a)
      [a: 1, a: 3]

  """
  @spec filter_by_keys(t(k, v), (k -> as_boolean(term))) :: t(k, v) when k: any, v: any
  def filter_by_keys(enum, f) do
    enum |> Enum.filter(&apply_to_key(&1, f))
  end

  @doc """
  Filters elements of `enum` by values using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.filter_by_values([a: 1, b: 2, a: 3], & &1 |> rem(2) == 0)
      [b: 2]

  """
  @spec filter_by_values(t(k, v), (v -> as_boolean(term))) :: t(k, v) when k: any, v: any
  def filter_by_values(enum, f) do
    enum |> Enum.filter(&apply_to_value(&1, f))
  end

  @doc """
  Executes `f` for each key in `enum`.

  ## Example

      iex> #{inspect(__MODULE__)}.each_key([a: 1, b: 2, a: 3], & send(self(), &1))
      iex> [:a, :b, :a] |> Enum.each(&receive do ^&1 -> :ok end)
      :ok

  """
  @spec each_key(t(k, v), (k -> any | no_return)) :: :ok when k: any, v: any
  def each_key(enum, f) do
    enum |> Enum.each(&apply_to_key(&1, f))
  end

  @doc """
  Executes `f` for each value in `enum`.

  ## Example

      iex> #{inspect(__MODULE__)}.each_value([a: 1, b: 2, a: 3], & send(self(), &1))
      iex> 1..3 |> Enum.each(&receive do ^&1 -> :ok end)
      :ok

  """
  @spec each_value(t(k, v), (v -> any | no_return)) :: :ok when k: any, v: any
  def each_value(enum, f) do
    enum |> Enum.each(&apply_to_value(&1, f))
  end

  @doc """
  Returns `true` if `f` returns truthy value for any key from `enum`, otherwise `false`.

  ## Example

      iex> #{inspect(__MODULE__)}.any_key?([a: 1, b: 2, a: 3], & &1 == :b)
      true
      iex> #{inspect(__MODULE__)}.any_key?([a: 1, b: 3, a: 5], & &1 == :c)
      false

  """
  @spec any_key?(t(k, v), (k -> as_boolean(term))) :: boolean when k: any, v: any
  def any_key?(enum, f \\ & &1) do
    enum |> Enum.any?(&apply_to_key(&1, f))
  end

  @doc """
  Returns `true` if `f` returns truthy value for any value from `enum`, otherwise `false`.

  ## Example

      iex> #{inspect(__MODULE__)}.any_value?([a: 1, b: 2, a: 3], & &1 |> rem(2) == 0)
      true
      iex> #{inspect(__MODULE__)}.any_value?([a: 1, b: 3, a: 5], & &1 |> rem(2) == 0)
      false

  """
  @spec any_value?(t(k, v), (v -> as_boolean(term))) :: boolean when k: any, v: any
  def any_value?(enum, f \\ & &1) do
    enum |> Enum.any?(&apply_to_value(&1, f))
  end

  defp apply_to_key({key, _value}, f), do: f.(key)
  defp apply_to_value({_key, value}, f), do: f.(value)
end
