defmodule Bunch.Map do
  @moduledoc """
  A bunch of helper functions for manipulating maps.
  """
  use Bunch

  @doc """
  Updates value at `key` in `map` and returns new value and updated map.

  Uses `Map.get_and_update/3` under the hood.

  ## Example

      iex> %{a: 1} |> #{inspect(__MODULE__)}.get_updated(:a, & &1+1)
      {2, %{a: 2}}

  """
  @spec get_updated(map, Map.key(), (Map.value() -> v)) :: {v, map} when v: Map.value()
  def get_updated(map, key, fun) do
    Map.get_and_update(map, key, fn a -> fun.(a) ~> {&1, &1} end)
  end

  @doc """
  Works like `get_updated/3`, but requires `map` to contain `key`.

  Uses `Map.get_and_update!/3` under the hood.

  ## Example

      iex> %{a: 1} |> #{inspect(__MODULE__)}.get_updated!(:a, & &1+1)
      {2, %{a: 2}}

  """
  @spec get_updated!(map, Map.key(), (Map.value() -> v)) :: {v, map} when v: Map.value()
  def get_updated!(map, key, fun) do
    Map.get_and_update!(map, key, fn a -> fun.(a) ~> {&1, &1} end)
  end

  @doc """
  Maps keys of `map` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_keys(%{1 => :a, 2 => :b}, & &1+1)
      %{2 => :a, 3 => :b}

  """
  @spec map_keys(%{k1 => v}, (k1 -> k2)) :: %{k2 => v} when k1: any, k2: any, v: any
  def map_keys(map, f) do
    map |> Enum.into(Map.new(), fn {key, value} -> {f.(key), value} end)
  end

  @doc """
  Maps values of `map` using function `f`.

  ## Example

      iex> #{inspect(__MODULE__)}.map_values(%{a: 1, b: 2}, & &1+1)
      %{a: 2, b: 3}

  """
  @spec map_values(%{k => v1}, (v1 -> v2)) :: %{k => v2} when k: any, v1: any, v2: any
  def map_values(map, f) do
    map |> Enum.into(Map.new(), fn {key, value} -> {key, f.(value)} end)
  end

  @doc """
  Moves value stored at `old_key` to `new_key`.

  If `old_key` is not present in `map`, `default_value` is stored at `new_key`.
  If `new_key` is present in `map`, it's value is overwritten.

  ## Examples

      iex> #{inspect(__MODULE__)}.move(%{a: 1, b: 2}, :a, :c, 3)
      %{b: 2, c: 1}
      iex> #{inspect(__MODULE__)}.move(%{a: 1, b: 2}, :a, :b, 3)
      %{b: 1}
      iex> #{inspect(__MODULE__)}.move(%{a: 1, b: 2}, :c, :b, 3)
      %{a: 1, b: 3}

  """
  @spec move(%{k => v}, old_key :: k, new_key :: k, default_value :: v) :: %{k => v}
        when k: any, v: any
  def move(map, old_key, new_key, default_value) do
    {value, map} = map |> Map.pop(old_key, default_value)
    map |> Map.put(new_key, value)
  end

  @doc """
  Works like `move/3`, but fails if either `old_key` is absent or `new_key` is present
  in `map`.

  ## Example

      iex> #{inspect(__MODULE__)}.move!(%{a: 1, b: 2}, :a, :c)
      %{b: 2, c: 1}

  """
  @spec move!(%{k => v}, old_key :: k, new_key :: k) :: %{k => v} | no_return
        when k: any, v: any
  def move!(map, old_key, new_key) do
    true = Map.has_key?(map, old_key) and not Map.has_key?(map, new_key)
    {value, map} = map |> Map.pop(old_key)
    map |> Map.put(new_key, value)
  end
end
