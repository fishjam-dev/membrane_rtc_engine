defmodule BiMap do
  @moduledoc """
  Bi-directional map implementation backed by two maps.

  > In computer science, a bidirectional map, or hash bag, is an associative data
  > structure in which the `(key, value)` pairs form a one-to-one correspondence.
  > Thus the binary relation is functional in each direction: `value` can also
  > act as a key to `key`. A pair `(a, b)` thus provides a unique coupling
  > between a `a` and `b` so that `b` can be found when `a` is used as a key and
  > `a` can be found when `b` is used as a key.
  >
  > ~[Wikipedia](https://en.wikipedia.org/wiki/Bidirectional_map)

  Entries in bimap do not follow any order.

  BiMaps do not impose any restriction on the key and value type: anything can be
  a key in a bimap, and also anything can be a value. As a bidirectional
  key-value structure, bimaps do not allow duplicated keys and values. This means
  it is not possible to store `[(A, B), (A, C)]` or `[(X, Z), (Y, Z)]` in
  the bimap. If you need to lift this restriction to only not allowing duplicated
  key-value pairs, check out `BiMultiMap`.

  Keys and values are compared using the exact-equality operator (`===`).

  ## Example

      iex> bm = BiMap.new(a: 1, b: 2)
      BiMap.new([a: 1, b: 2])
      iex> BiMap.get(bm, :a)
      1
      iex> BiMap.get_key(bm, 2)
      :b
      iex> BiMap.put(bm, :a, 3)
      BiMap.new([a: 3, b: 2])
      iex> BiMap.put(bm, :c, 2)
      BiMap.new([a: 1, c: 2])

  ## Protocols

  `BiMap` implements `Enumerable`, `Collectable` and `Inspect` protocols.
  """

  @typedoc "Key type"
  @type k :: term

  @typedoc "Value type"
  @type v :: term

  @typedoc false
  @opaque internal_keys(k, v) :: %{optional(k) => v}

  @typedoc false
  @opaque internal_values(k, v) :: %{optional(v) => k}

  @type t(k, v) :: %BiMap{
          keys: internal_keys(k, v),
          values: internal_values(k, v)
        }

  @type t :: t(term, term)

  defstruct keys: %{}, values: %{}

  @doc """
  Creates a new bimap.

  ## Examples

      iex> BiMap.new
      BiMap.new([])
  """
  @spec new :: t
  def new, do: %BiMap{}

  @doc """
  Creates a bimap from `enumerable` of key-value pairs.

  Duplicated pairs are removed; the latest one prevails.

  ## Examples

      iex> BiMap.new([a: "foo", b: "bar"])
      BiMap.new([a: "foo", b: "bar"])
  """
  @spec new(Enum.t()) :: t
  def new(enumerable)
  def new(%BiMap{} = bimap), do: bimap

  def new(enum) do
    Enum.reduce(enum, new(), fn pair, bimap ->
      BiMap.put(bimap, pair)
    end)
  end

  @doc """
  Creates a bimap from `enumerable` via transform function returning key-value
  pairs.

  ## Examples

      iex> BiMap.new([1, 2, 1], fn x -> {x, x * 2} end)
      BiMap.new([{1, 2}, {2, 4}])
  """
  @spec new(Enum.t(), (term -> {k, v})) :: t
  def new(enumerable, transform)

  def new(enum, f) do
    Enum.reduce(enum, new(), fn term, bimap ->
      BiMap.put(bimap, f.(term))
    end)
  end

  @doc """
  Returns the number of elements in `bimap`.

  The size of a bimap is the number of key-value pairs that the map contains.

  ## Examples

      iex> BiMap.size(BiMap.new)
      0

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.size(bimap)
      2
  """
  @spec size(t) :: non_neg_integer
  def size(bimap)

  def size(%BiMap{keys: keys}) do
    map_size(keys)
  end

  @doc """
  Returns `key ➜ value` mapping of `bimap`.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.left(bimap)
      %{a: "foo", b: "bar"}
  """
  @spec left(t) :: %{k => v}
  def left(bimap)
  def left(%BiMap{keys: keys}), do: keys

  @doc """
  Returns `value ➜ key` mapping of `bimap`.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.right(bimap)
      %{"foo" => :a, "bar" => :b}
  """
  @spec right(t) :: %{v => k}
  def right(bimap)
  def right(%BiMap{values: values}), do: values

  @doc """
  Returns all keys from `bimap`.

  ## Examples

      iex> bimap = BiMap.new([a: 1, b: 2])
      iex> BiMap.keys(bimap)
      [:a, :b]
  """
  @spec keys(t) :: [k]
  def keys(bimap)
  def keys(%BiMap{keys: keys}), do: Map.keys(keys)

  @doc """
  Returns all values from `bimap`.

  ## Examples

      iex> bimap = BiMap.new([a: 1, b: 2])
      iex> BiMap.values(bimap)
      [1, 2]
  """
  @spec values(t) :: [v]
  def values(bimap)
  def values(%BiMap{values: values}), do: Map.keys(values)

  @doc """
  Checks if `bimap` contains `{key, value}` pair.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.member?(bimap, :a, "foo")
      true
      iex> BiMap.member?(bimap, :a, "bar")
      false
  """
  @spec member?(t, k, v) :: boolean
  def member?(bimap, key, value)

  def member?(%BiMap{keys: keys}, key, value) do
    Map.has_key?(keys, key) and keys[key] === value
  end

  @doc """
  Convenience shortcut for `member?/3`.
  """
  @spec member?(t, {k, v}) :: boolean
  def member?(bimap, kv)
  def member?(bimap, {key, value}), do: member?(bimap, key, value)

  @doc """
  Checks if `bimap` contains `key`.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.has_key?(bimap, :a)
      true
      iex> BiMap.has_key?(bimap, :x)
      false
  """
  @spec has_key?(t, k) :: boolean
  def has_key?(bimap, key)

  def has_key?(%BiMap{keys: keys}, left) do
    Map.has_key?(keys, left)
  end

  @doc """
  Checks if `bimap` contains `value`.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.has_value?(bimap, "foo")
      true
      iex> BiMap.has_value?(bimap, "moo")
      false
  """
  @spec has_value?(t, v) :: boolean
  def has_value?(bimap, value)

  def has_value?(%BiMap{values: values}, value) do
    Map.has_key?(values, value)
  end

  @doc """
  Checks if two bimaps are equal.

  Two bimaps are considered to be equal if they contain the same keys and those
  keys contain the same values.

  ## Examples

      iex> Map.equal?(BiMap.new([a: 1, b: 2]), BiMap.new([b: 2, a: 1]))
      true
      iex> Map.equal?(BiMap.new([a: 1, b: 2]), BiMap.new([b: 1, a: 2]))
      false
  """
  @spec equal?(t, t) :: boolean
  def equal?(bimap1, bimap2)

  def equal?(%BiMap{keys: keys1}, %BiMap{keys: keys2}) do
    Map.equal?(keys1, keys2)
  end

  @doc """
  Gets the value for specific `key` in `bimap`

  If `key` is present in `bimap` with value `value`, then `value` is returned.
  Otherwise, `default` is returned (which is `nil` unless specified otherwise).

  ## Examples

      iex> BiMap.get(BiMap.new(), :a)
      nil
      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.get(bimap, :a)
      1
      iex> BiMap.get(bimap, :b)
      nil
      iex> BiMap.get(bimap, :b, 3)
      3
  """
  @spec get(t, k, v) :: v
  def get(bimap, key, default \\ nil)

  def get(%BiMap{keys: keys}, key, default) do
    Map.get(keys, key, default)
  end

  @doc """
  Gets the key for specific `value` in `bimap`

  This function is exact mirror of `get/3`.

  ## Examples

      iex> BiMap.get_key(BiMap.new, 1)
      nil
      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.get_key(bimap, 1)
      :a
      iex> BiMap.get_key(bimap, 2)
      nil
      iex> BiMap.get_key(bimap, 2, :b)
      :b
  """
  @spec get_key(t, v, k) :: k
  def get_key(bimap, value, default \\ nil)

  def get_key(%BiMap{values: values}, value, default) do
    Map.get(values, value, default)
  end

  @doc """
  Fetches the value for specific `key` in `bimap`

  If `key` is present in `bimap` with value `value`, then `{:ok, value}` is
  returned. Otherwise, `:error` is returned.

  ## Examples

      iex> BiMap.fetch(BiMap.new(), :a)
      :error
      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.fetch(bimap, :a)
      {:ok, 1}
      iex> BiMap.fetch(bimap, :b)
      :error
  """
  @spec fetch(t, k) :: {:ok, v} | :error
  def fetch(bimap, key)

  def fetch(%BiMap{keys: keys}, key) do
    Map.fetch(keys, key)
  end

  @doc """
  Fetches the value for specific `key` in `bimap`.

  Raises `ArgumentError` if the key is absent.

  ## Examples

      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.fetch!(bimap, :a)
      1
  """
  @spec fetch!(t, k) :: v
  def fetch!(bimap, key)

  def fetch!(bimap, key) do
    case fetch(bimap, key) do
      {:ok, value} -> value
      :error -> raise ArgumentError, "key #{inspect(key)} not found in: #{inspect(bimap)}"
    end
  end

  @doc """
  Fetches the key for specific `value` in `bimap`

  This function is exact mirror of `fetch/2`.

  ## Examples

      iex> BiMap.fetch_key(BiMap.new, 1)
      :error
      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.fetch_key(bimap, 1)
      {:ok, :a}
      iex> BiMap.fetch_key(bimap, 2)
      :error
  """
  @spec fetch_key(t, v) :: {:ok, k} | :error
  def fetch_key(bimap, value)

  def fetch_key(%BiMap{values: values}, value) do
    Map.fetch(values, value)
  end

  @doc """
  Fetches the key for specific `value` in `bimap`.

  Raises `ArgumentError` if the value is absent. This function is exact mirror of `fetch!/2`.

  ## Examples

      iex> bimap = BiMap.new([a: 1])
      iex> BiMap.fetch_key!(bimap, 1)
      :a
  """
  @spec fetch_key!(t, v) :: k
  def fetch_key!(bimap, value)

  def fetch_key!(bimap, value) do
    case fetch_key(bimap, value) do
      {:ok, key} -> key
      :error -> raise ArgumentError, "value #{inspect(value)} not found in: #{inspect(bimap)}"
    end
  end

  @doc """
  Inserts `{key, value}` pair into `bimap`.

  If either `key` or `value` is already in `bimap`, any overlapping bindings are
  deleted.

  ## Examples

      iex> bimap = BiMap.new
      BiMap.new([])
      iex> bimap = BiMap.put(bimap, :a, 0)
      BiMap.new([a: 0])
      iex> bimap = BiMap.put(bimap, :a, 1)
      BiMap.new([a: 1])
      iex> BiMap.put(bimap, :b, 1)
      BiMap.new([b: 1])
  """
  @spec put(t, k, v) :: t
  def put(%BiMap{} = bimap, key, value) do
    %{keys: keys, values: values} = bimap |> BiMap.delete_key(key) |> BiMap.delete_value(value)
    %{bimap | keys: Map.put(keys, key, value), values: Map.put(values, value, key)}
  end

  @doc """
  Convenience shortcut for `put/3`
  """
  @spec put(t, {k, v}) :: t
  def put(bimap, kv)
  def put(bimap, {key, value}), do: put(bimap, key, value)

  @doc """
  Inserts `{key, value}` pair into `bimap` if `key` is not already in `bimap`.

  If `key` already exists in `bimap`, `bimap` is returned unchanged.

  If `key` does not exist and `value` is already in `bimap`, any overlapping bindings are
  deleted.

  ## Examples

      iex> bimap = BiMap.new
      BiMap.new([])
      iex> bimap = BiMap.put_new_key(bimap, :a, 0)
      BiMap.new([a: 0])
      iex> bimap = BiMap.put_new_key(bimap, :a, 1)
      BiMap.new([a: 0])
      iex> BiMap.put_new_key(bimap, :b, 1)
      BiMap.new([a: 0, b: 1])
      iex> BiMap.put_new_key(bimap, :c, 1)
      BiMap.new([a: 0, c: 1])
  """
  @spec put_new_key(t, k, v) :: t
  def put_new_key(%BiMap{} = bimap, key, value) do
    if BiMap.has_key?(bimap, key) do
      bimap
    else
      put(bimap, key, value)
    end
  end

  @doc """
  Inserts `{key, value}` pair into `bimap` if `value` is not already in `bimap`.

  If `value` already exists in `bimap`, `bimap` is returned unchanged.

  If `value` does not exist and `key` is already in `bimap`, any overlapping bindings are
  deleted.

  ## Examples

      iex> bimap = BiMap.new
      BiMap.new([])
      iex> bimap = BiMap.put_new_value(bimap, :a, 0)
      BiMap.new([a: 0])
      iex> bimap = BiMap.put_new_value(bimap, :a, 1)
      BiMap.new([a: 1])
      iex> BiMap.put_new_value(bimap, :b, 1)
      BiMap.new([a: 1])
      iex> BiMap.put_new_value(bimap, :c, 2)
      BiMap.new([a: 1, c: 2])
  """
  @spec put_new_value(t, k, v) :: t
  def put_new_value(%BiMap{} = bimap, key, value) do
    if BiMap.has_value?(bimap, value) do
      bimap
    else
      put(bimap, key, value)
    end
  end

  @doc """
  Deletes `{key, value}` pair from `bimap`.

  If the `key` does not exist, or `value` does not match, returns `bimap`
  unchanged.

  ## Examples

      iex> bimap = BiMap.new([a: 1, b: 2])
      iex> BiMap.delete(bimap, :b, 2)
      BiMap.new([a: 1])
      iex> BiMap.delete(bimap, :c, 3)
      BiMap.new([a: 1, b: 2])
      iex> BiMap.delete(bimap, :b, 3)
      BiMap.new([a: 1, b: 2])
  """
  @spec delete(t, k, v) :: t
  def delete(%BiMap{keys: keys, values: values} = bimap, key, value) do
    case Map.fetch(keys, key) do
      {:ok, ^value} ->
        %{bimap | keys: Map.delete(keys, key), values: Map.delete(values, value)}

      _ ->
        bimap
    end
  end

  @doc """
  Deletes `{key, _}` pair from `bimap`.

  If the `key` does not exist, returns `bimap` unchanged.

  ## Examples

      iex> bimap = BiMap.new([a: 1, b: 2])
      iex> BiMap.delete_key(bimap, :b)
      BiMap.new([a: 1])
      iex> BiMap.delete_key(bimap, :c)
      BiMap.new([a: 1, b: 2])
  """
  @spec delete_key(t, k) :: t
  def delete_key(%BiMap{keys: keys, values: values} = bimap, key) do
    case Map.fetch(keys, key) do
      {:ok, value} ->
        %{bimap | keys: Map.delete(keys, key), values: Map.delete(values, value)}

      :error ->
        bimap
    end
  end

  @doc """
  Deletes `{_, value}` pair from `bimap`.

  If the `value` does not exist, returns `bimap` unchanged.

  ## Examples

      iex> bimap = BiMap.new([a: 1, b: 2])
      iex> BiMap.delete_value(bimap, 2)
      BiMap.new([a: 1])
      iex> BiMap.delete_value(bimap, 3)
      BiMap.new([a: 1, b: 2])
  """
  @spec delete_value(t, v) :: t
  def delete_value(%BiMap{keys: keys, values: values} = bimap, value) do
    case Map.fetch(values, value) do
      {:ok, key} ->
        %{bimap | keys: Map.delete(keys, key), values: Map.delete(values, value)}

      :error ->
        bimap
    end
  end

  @doc """
  Convenience shortcut for `delete/3`.
  """
  @spec delete(t, {k, v}) :: t
  def delete(bimap, kv)
  def delete(bimap, {key, value}), do: delete(bimap, key, value)

  @doc """
  Returns list of unique key-value pairs in `bimap`.

  ## Examples

      iex> bimap = BiMap.new([a: "foo", b: "bar"])
      iex> BiMap.to_list(bimap)
      [a: "foo", b: "bar"]
  """
  @spec to_list(t) :: [{k, v}]
  def to_list(bimap)

  def to_list(%BiMap{keys: keys}) do
    Map.to_list(keys)
  end

  defimpl Enumerable do
    def reduce(bimap, acc, fun) do
      Enumerable.List.reduce(BiMap.to_list(bimap), acc, fun)
    end

    def member?(bimap, val) do
      {:ok, BiMap.member?(bimap, val)}
    end

    def count(bimap) do
      {:ok, BiMap.size(bimap)}
    end

    def slice(_bimap) do
      {:error, __MODULE__}
    end
  end

  defimpl Collectable do
    def into(original) do
      {original,
       fn
         bimap, {:cont, pair} -> BiMap.put(bimap, pair)
         bimap, :done -> bimap
         _, :halt -> :ok
       end}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(bimap, opts) do
      concat(["BiMap.new(", to_doc(BiMap.to_list(bimap), opts), ")"])
    end
  end
end
