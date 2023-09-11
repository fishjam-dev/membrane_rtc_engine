defmodule Bunch.KVList do
  @deprecated "Use `Bunch.KVEnum` instead"
  @moduledoc """
  A bunch of helper functions for manipulating key-value lists (including keyword
  lists).

  Key-value lists are represented as lists of 2-element tuples, where the first
  element of each tuple is a key, and the second is a value.
  """

  @type t(key, value) :: [{key, value}]

  @doc """
  Maps keys of `list` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_keys([{1, :a}, {2, :b}], & &1+1)
      [{2, :a}, {3, :b}]

  """
  @spec map_keys(t(k1, v), (k1 -> k2)) :: t(k2, v) when k1: any, k2: any, v: any
  def map_keys(list, f) do
    list |> Enum.map(fn {key, value} -> {f.(key), value} end)
  end

  @doc """
  Maps values of `list` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_values([a: 1, b: 2], & &1+1)
      [a: 2, b: 3]

  """
  @spec map_values(t(k, v1), (v1 -> v2)) :: t(k, v2) when k: any, v1: any, v2: any
  def map_values(list, f) do
    list |> Enum.map(fn {key, value} -> {key, f.(value)} end)
  end

  @doc """
  Filters elements of `list` by keys using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.filter_by_keys([a: 1, b: 2, a: 3], & &1 == :a)
      [a: 1, a: 3]

  """
  @spec filter_by_keys(t(k, v), (k -> as_boolean(term))) :: t(k, v) when k: any, v: any
  def filter_by_keys(list, f) do
    list |> Enum.filter(&apply_to_key(&1, f))
  end

  @doc """
  Filters elements of `list` by values using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.filter_by_values([a: 1, b: 2, a: 3], & &1 |> rem(2) == 0)
      [b: 2]

  """
  @spec filter_by_values(t(k, v), (v -> as_boolean(term))) :: t(k, v) when k: any, v: any
  def filter_by_values(list, f) do
    list |> Enum.filter(&apply_to_value(&1, f))
  end

  @doc """
  Executes `f` for each key in `list`.

  ## Example

      iex> #{inspect(__MODULE__)}.each_key([a: 1, b: 2, a: 3], & send(self(), &1))
      iex> [:a, :b, :a] |> Enum.each(&receive do ^&1 -> :ok end)
      :ok

  """
  @spec each_key(t(k, v), (k -> any | no_return)) :: :ok when k: any, v: any
  def each_key(list, f) do
    list |> Enum.each(&apply_to_key(&1, f))
  end

  @doc """
  Executes `f` for each value in `list`.

  ## Example

      iex> #{inspect(__MODULE__)}.each_value([a: 1, b: 2, a: 3], & send(self(), &1))
      iex> 1..3 |> Enum.each(&receive do ^&1 -> :ok end)
      :ok

  """
  @spec each_value(t(k, v), (v -> any | no_return)) :: :ok when k: any, v: any
  def each_value(list, f) do
    list |> Enum.each(&apply_to_value(&1, f))
  end

  @doc """
  Returns `true` if `f` returns truthy value for any key from `list`, otherwise `false`.

  ## Example

      iex> #{inspect(__MODULE__)}.any_key?([a: 1, b: 2, a: 3], & &1 == :b)
      true
      iex> #{inspect(__MODULE__)}.any_key?([a: 1, b: 3, a: 5], & &1 == :c)
      false

  """
  @spec any_key?(t(k, v), (k -> as_boolean(term))) :: boolean when k: any, v: any
  def any_key?(list, f) do
    list |> Enum.any?(&apply_to_key(&1, f))
  end

  @doc """
  Returns `true` if `f` returns truthy value for any value from `list`, otherwise `false`.

  ## Example

      iex> #{inspect(__MODULE__)}.any_value?([a: 1, b: 2, a: 3], & &1 |> rem(2) == 0)
      true
      iex> #{inspect(__MODULE__)}.any_value?([a: 1, b: 3, a: 5], & &1 |> rem(2) == 0)
      false

  """
  @spec any_value?(t(k, v), (v -> as_boolean(term))) :: boolean when k: any, v: any
  def any_value?(list, f) do
    list |> Enum.any?(&apply_to_value(&1, f))
  end

  defp apply_to_key({key, _value}, f), do: f.(key)
  defp apply_to_value({_key, value}, f), do: f.(value)
end
