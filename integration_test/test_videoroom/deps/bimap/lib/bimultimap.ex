defmodule BiMultiMap do
  @moduledoc """
  Bi-directional multimap implementation backed by two multimaps.

  Entries in bimap do not follow any order.

  BiMultiMaps do not impose any restriction on the key and value type: anything
  can be a key in a bimap, and also anything can be a value.

  BiMultiMaps differ from `BiMap`s by disallowing duplicates only among key-value
  pairs, not among keys and values separately. This means it is possible to store
  `[(A, B), (A, C)]` or `[(X, Z), (Y, Z)]` in BiMultiMap.

  Keys and values are compared using the exact-equality operator (`===`).

  ## Example

      iex> mm = BiMultiMap.new(a: 1, b: 2, b: 1)
      BiMultiMap.new([a: 1, b: 1, b: 2])
      iex> BiMultiMap.get(mm, :a)
      [1]
      iex> BiMultiMap.get_keys(mm, 1)
      [:a, :b]
      iex> BiMultiMap.put(mm, :a, 3)
      BiMultiMap.new([a: 1, a: 3, b: 1, b: 2])

  ## Protocols

  `BiMultiMap` implements `Enumerable`, `Collectable` and `Inspect` protocols.
  """

  @typedoc "Key type"
  @type k :: term

  @typedoc "Value type"
  @type v :: term

  @typedoc false
  @opaque internal_keys(k, v) :: %{optional(k) => MapSet.t(v)}

  @typedoc false
  @opaque internal_values(k, v) :: %{optional(v) => MapSet.t(k)}

  @typedoc false
  @opaque internal_size :: non_neg_integer

  @type t(k, v) :: %BiMultiMap{
          keys: internal_keys(k, v),
          values: internal_values(k, v),
          size: internal_size
        }

  @type t :: t(term, term)

  defstruct keys: %{}, values: %{}, size: 0

  @doc """
  Creates a new bimultimap.

  ## Examples

      iex> BiMultiMap.new
      BiMultiMap.new([])
  """
  @spec new :: t
  def new, do: %BiMultiMap{}

  @doc """
  Creates a bimultimap from `enumerable` of key-value pairs.

  Duplicated pairs are removed; the latest one prevails.

  ## Examples

      iex> BiMultiMap.new([a: 1, a: 2])
      BiMultiMap.new([a: 1, a: 2])
  """
  @spec new(Enum.t()) :: t
  def new(enumerable)
  def new(%BiMultiMap{} = bimultimap), do: bimultimap

  def new(enum) do
    Enum.reduce(enum, new(), fn pair, bimultimap ->
      BiMultiMap.put(bimultimap, pair)
    end)
  end

  @doc """
  Creates a bimultimap from `enumerable` via transform function returning
  key-value pairs.

  ## Examples

      iex> BiMultiMap.new([1, 2, 1], fn x -> {x, x * 2} end)
      BiMultiMap.new([{1, 2}, {2, 4}])
  """
  @spec new(Enum.t(), (term -> {k, v})) :: t
  def new(enumerable, transform)

  def new(enum, f) do
    Enum.reduce(enum, new(), fn term, bimultimap ->
      BiMultiMap.put(bimultimap, f.(term))
    end)
  end

  @doc """
  Returns the number of elements in `bimultimap`.

  The size of a bimultimap is the number of key-value pairs that the map
  contains.

  ## Examples

      iex> BiMultiMap.size(BiMultiMap.new)
      0

      iex> bimultimap = BiMultiMap.new([a: "foo", a: "bar"])
      iex> BiMultiMap.size(bimultimap)
      2
  """
  @spec size(t) :: non_neg_integer
  def size(bimultimap)
  def size(%BiMultiMap{size: size}), do: size

  @doc """
  Returns `key ➜ [value]` mapping of `bimultimap`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", b: "bar", b: "moo"])
      iex> BiMultiMap.left(bimultimap)
      %{a: ["foo"], b: ["bar", "moo"]}
  """
  @spec left(t) :: %{k => [v]}
  def left(bimultimap)

  def left(%BiMultiMap{keys: keys}) do
    for {k, vs} <- keys, into: %{} do
      {k, MapSet.to_list(vs)}
    end
  end

  @doc """
  Returns `value ➜ key` mapping of `bimultimap`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", b: "bar", c: "bar"])
      iex> BiMultiMap.right(bimultimap)
      %{"foo" => [:a], "bar" => [:b, :c]}
  """
  @spec right(t) :: %{v => [k]}
  def right(bimultimap)

  def right(%BiMultiMap{values: values}) do
    for {v, ks} <- values, into: %{} do
      {v, MapSet.to_list(ks)}
    end
  end

  @doc """
  Returns all unique keys from `bimultimap`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, b: 2, b: 3])
      iex> BiMultiMap.keys(bimultimap)
      [:a, :b]
  """
  @spec keys(t) :: [k]
  def keys(bimultimap)
  def keys(%BiMultiMap{keys: keys}), do: Map.keys(keys)

  @doc """
  Returns all unique values from `bimultimap`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, b: 2, c: 2])
      iex> BiMultiMap.values(bimultimap)
      [1, 2]
  """
  @spec values(t) :: [v]
  def values(bimultimap)
  def values(%BiMultiMap{values: values}), do: Map.keys(values)

  @doc """
  Checks if `bimultimap` contains `{key, value}` pair.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", a: "moo", b: "bar"])
      iex> BiMultiMap.member?(bimultimap, :a, "foo")
      true
      iex> BiMultiMap.member?(bimultimap, :a, "moo")
      true
      iex> BiMultiMap.member?(bimultimap, :a, "bar")
      false
  """
  @spec member?(t, k, v) :: boolean
  def member?(bimultimap, key, value)

  def member?(%BiMultiMap{keys: keys}, key, value) do
    Map.has_key?(keys, key) and value in keys[key]
  end

  @doc """
  Convenience shortcut for `member?/3`.
  """
  @spec member?(t, {k, v}) :: boolean
  def member?(bimultimap, kv)
  def member?(bimultimap, {key, value}), do: member?(bimultimap, key, value)

  @doc """
  Checks if `bimultimap` contains `key`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", b: "bar"])
      iex> BiMultiMap.has_key?(bimultimap, :a)
      true
      iex> BiMultiMap.has_key?(bimultimap, :x)
      false
  """
  @spec has_key?(t, k) :: boolean
  def has_key?(bimultimap, key)

  def has_key?(%BiMultiMap{keys: keys}, left) do
    Map.has_key?(keys, left)
  end

  @doc """
  Checks if `bimultimap` contains `value`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", b: "bar"])
      iex> BiMultiMap.has_value?(bimultimap, "foo")
      true
      iex> BiMultiMap.has_value?(bimultimap, "moo")
      false
  """
  @spec has_value?(t, v) :: boolean
  def has_value?(bimultimap, value)

  def has_value?(%BiMultiMap{values: values}, value) do
    Map.has_key?(values, value)
  end

  @doc """
  Checks if two bimultimaps are equal.

  Two bimultimaps are considered to be equal if they contain the same keys and
  those keys are bound with the same values.

  ## Examples

      iex> Map.equal?(BiMultiMap.new([a: 1, b: 2, b: 3]), BiMultiMap.new([b: 2, b: 3, a: 1]))
      true
      iex> Map.equal?(BiMultiMap.new([a: 1, b: 2, b: 3]), BiMultiMap.new([b: 1, b: 3, a: 2]))
      false
  """
  @spec equal?(t, t) :: boolean
  def equal?(bimultimap1, bimultimap2)

  def equal?(%BiMultiMap{keys: keys1}, %BiMultiMap{keys: keys2}) do
    Map.equal?(keys1, keys2)
  end

  @doc """
  Gets all values for specific `key` in `bimultimap`

  If `key` is present in `bimultimap` with values `values`, then `values` are
  returned. Otherwise, `default` is returned (which is `[]` unless specified
  otherwise).

  ## Examples

      iex> BiMultiMap.get(BiMultiMap.new(), :a)
      []
      iex> bimultimap = BiMultiMap.new([a: 1, c: 1, c: 2])
      iex> BiMultiMap.get(bimultimap, :a)
      [1]
      iex> BiMultiMap.get(bimultimap, :b)
      []
      iex> BiMultiMap.get(bimultimap, :b, 3)
      3
      iex> BiMultiMap.get(bimultimap, :c)
      [1, 2]
  """
  @spec get(t, k, any) :: [v] | any
  def get(bimultimap, key, default \\ [])

  def get(%BiMultiMap{keys: keys}, key, default) do
    case Map.fetch(keys, key) do
      {:ok, values} -> MapSet.to_list(values)
      :error -> default
    end
  end

  @doc """
  Gets all keys for specific `value` in `bimultimap`

  This function is exact mirror of `get/3`.

  ## Examples

      iex> BiMultiMap.get_keys(BiMultiMap.new, 1)
      []
      iex> bimultimap = BiMultiMap.new([a: 1, c: 3, d: 3])
      iex> BiMultiMap.get_keys(bimultimap, 1)
      [:a]
      iex> BiMultiMap.get_keys(bimultimap, 2)
      []
      iex> BiMultiMap.get_keys(bimultimap, 2, :b)
      :b
      iex> BiMultiMap.get_keys(bimultimap, 3)
      [:c, :d]
  """
  @spec get_keys(t, v, any) :: [k] | any
  def get_keys(bimultimap, value, default \\ [])

  def get_keys(%BiMultiMap{values: values}, value, default) do
    case Map.fetch(values, value) do
      {:ok, keys} -> MapSet.to_list(keys)
      :error -> default
    end
  end

  @doc """
  Fetches all values for specific `key` in `bimultimap`

  If `key` is present in `bimultimap` with values `values`, then `{:ok, values}`
  is returned. Otherwise, `:error` is returned.

  ## Examples

      iex> BiMultiMap.fetch(BiMultiMap.new(), :a)
      :error
      iex> bimultimap = BiMultiMap.new([a: 1, c: 1, c: 2])
      iex> BiMultiMap.fetch(bimultimap, :a)
      {:ok, [1]}
      iex> BiMultiMap.fetch(bimultimap, :b)
      :error
      iex> BiMultiMap.fetch(bimultimap, :c)
      {:ok, [1, 2]}
  """
  @spec fetch(t, k) :: {:ok, [v]} | :error
  def fetch(bimultimap, key)

  def fetch(%BiMultiMap{keys: keys}, key) do
    case Map.fetch(keys, key) do
      {:ok, values} -> {:ok, MapSet.to_list(values)}
      :error -> :error
    end
  end

  @doc """
  Fetches all values for specific `key` in `bimultimap`

  Raises `ArgumentError` if the key is absent.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, c: 1, c: 2])
      iex> BiMultiMap.fetch!(bimultimap, :a)
      [1]
      iex> BiMultiMap.fetch!(bimultimap, :c)
      [1, 2]
  """
  @spec fetch!(t, k) :: [v]
  def fetch!(bimultimap, key)

  def fetch!(bimultimap, key) do
    case fetch(bimultimap, key) do
      {:ok, values} -> values
      :error -> raise ArgumentError, "key #{inspect(key)} not found in: #{inspect(bimultimap)}"
    end
  end

  @doc """
  Fetches all keys for specific `value` in `bimultimap`

  This function is exact mirror of `fetch/2`.

  ## Examples

      iex> BiMultiMap.fetch_keys(BiMultiMap.new, 1)
      :error
      iex> bimultimap = BiMultiMap.new([a: 1, c: 3, d: 3])
      iex> BiMultiMap.fetch_keys(bimultimap, 1)
      {:ok, [:a]}
      iex> BiMultiMap.fetch_keys(bimultimap, 2)
      :error
      iex> BiMultiMap.fetch_keys(bimultimap, 3)
      {:ok, [:c, :d]}
  """
  @spec fetch_keys(t, v) :: {:ok, [k]} | :error
  def fetch_keys(bimultimap, value)

  def fetch_keys(%BiMultiMap{values: values}, value) do
    case Map.fetch(values, value) do
      {:ok, keys} -> {:ok, MapSet.to_list(keys)}
      :error -> :error
    end
  end

  @doc """
  Fetches all keys for specific `value` in `bimultimap`

  Raises `ArgumentError` if the key is absent. This function is exact mirror of `fetch!/2`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, c: 3, d: 3])
      iex> BiMultiMap.fetch_keys!(bimultimap, 1)
      [:a]
      iex> BiMultiMap.fetch_keys!(bimultimap, 3)
      [:c, :d]
  """
  @spec fetch_keys!(t, v) :: [k]
  def fetch_keys!(bimultimap, value)

  def fetch_keys!(bimultimap, value) do
    case fetch_keys(bimultimap, value) do
      {:ok, keys} ->
        keys

      :error ->
        raise ArgumentError, "value #{inspect(value)} not found in: #{inspect(bimultimap)}"
    end
  end

  @doc """
  Inserts `{key, value}` pair into `bimultimap`.

  If `{key, value}` is already in `bimultimap`, it is deleted.

  ## Examples

      iex> bimultimap = BiMultiMap.new
      BiMultiMap.new([])
      iex> bimultimap = BiMultiMap.put(bimultimap, :a, 1)
      BiMultiMap.new([a: 1])
      iex> bimultimap = BiMultiMap.put(bimultimap, :a, 2)
      BiMultiMap.new([a: 1, a: 2])
      iex> BiMultiMap.put(bimultimap, :b, 2)
      BiMultiMap.new([a: 1, a: 2, b: 2])
  """
  @spec put(t, k, v) :: t
  def put(
        %BiMultiMap{keys: keys, values: values, size: size} = bimultimap,
        key,
        value
      ) do
    {upd, keys} = put_side(keys, key, value)
    {^upd, values} = put_side(values, value, key)

    size =
      if upd do
        size + 1
      else
        size
      end

    %{bimultimap | keys: keys, values: values, size: size}
  end

  defp put_side(keys, key, value) do
    Map.get_and_update(keys, key, fn
      nil -> {true, MapSet.new([value])}
      set -> {!MapSet.member?(set, value), MapSet.put(set, value)}
    end)
  end

  @doc """
  Convenience shortcut for `put/3`
  """
  @spec put(t, {k, v}) :: t
  def put(bimultimap, kv)
  def put(bimultimap, {key, value}), do: put(bimultimap, key, value)

  @doc """
  Deletes `{key, value}` pair from `bimultimap`.

  If the `key` does not exist, or `value` does not match, returns `bimultimap`
  unchanged.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, b: 2, c: 2])
      iex> BiMultiMap.delete(bimultimap, :b, 2)
      BiMultiMap.new([a: 1, c: 2])
      iex> BiMultiMap.delete(bimultimap, :c, 3)
      BiMultiMap.new([a: 1, b: 2, c: 2])
  """
  @spec delete(t, k, v) :: t
  def delete(
        %BiMultiMap{keys: keys, values: values, size: size} = bimultimap,
        key,
        value
      ) do
    {upd, keys} = delete_side(keys, key, value)
    {^upd, values} = delete_side(values, value, key)

    size =
      if upd do
        size - 1
      else
        size
      end

    %{bimultimap | keys: keys, values: values, size: size}
  end

  defp delete_side(keys, key, value) do
    case Map.fetch(keys, key) do
      {:ok, set} ->
        upd = MapSet.member?(set, value)
        set = MapSet.delete(set, value)

        keys =
          if MapSet.size(set) == 0 do
            Map.delete(keys, key)
          else
            put_in(keys[key], set)
          end

        {upd, keys}

      :error ->
        {false, keys}
    end
  end

  @doc """
  Deletes `{key, _}` pair from `bimultimap`.

  If the `key` does not exist, returns `bimultimap` unchanged.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, b: 2, b: 3])
      iex> BiMultiMap.delete_key(bimultimap, :b)
      BiMultiMap.new([a: 1])
      iex> BiMultiMap.delete_key(bimultimap, :c)
      BiMultiMap.new([a: 1, b: 2, b: 3])
  """
  @spec delete_key(t, k) :: t
  def delete_key(%BiMultiMap{keys: keys} = bimultimap, key) do
    case Map.fetch(keys, key) do
      {:ok, values} ->
        Enum.reduce(values, bimultimap, fn value, map ->
          delete(map, key, value)
        end)

      :error ->
        bimultimap
    end
  end

  @doc """
  Deletes `{_, value}` pair from `bimultimap`.

  If the `value` does not exist, returns `bimultimap` unchanged.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: 1, b: 2, c: 1])
      iex> BiMultiMap.delete_value(bimultimap, 1)
      BiMultiMap.new([b: 2])
      iex> BiMultiMap.delete_value(bimultimap, 3)
      BiMultiMap.new([a: 1, b: 2, c: 1])
  """
  @spec delete_value(t, v) :: t
  def delete_value(%BiMultiMap{values: values} = bimultimap, value) do
    case Map.fetch(values, value) do
      {:ok, keys} ->
        Enum.reduce(keys, bimultimap, fn key, map ->
          delete(map, key, value)
        end)

      :error ->
        bimultimap
    end
  end

  @doc """
  Convenience shortcut for `delete/3`.
  """
  @spec delete(t, {k, v}) :: t
  def delete(bimultimap, kv)
  def delete(bimultimap, {key, value}), do: delete(bimultimap, key, value)

  @doc """
  Returns list of unique key-value pairs in `bimultimap`.

  ## Examples

      iex> bimultimap = BiMultiMap.new([a: "foo", b: "bar"])
      iex> BiMultiMap.to_list(bimultimap)
      [a: "foo", b: "bar"]
  """
  @spec to_list(t) :: [{k, v}]
  def to_list(bimultimap)

  def to_list(%BiMultiMap{keys: keys}) do
    for {k, vs} <- keys, v <- vs do
      {k, v}
    end
  end

  defimpl Enumerable do
    def reduce(bimultimap, acc, fun) do
      Enumerable.List.reduce(BiMultiMap.to_list(bimultimap), acc, fun)
    end

    def member?(bimultimap, val) do
      {:ok, BiMultiMap.member?(bimultimap, val)}
    end

    def count(bimultimap) do
      {:ok, BiMultiMap.size(bimultimap)}
    end

    def slice(_bimultimap) do
      {:error, __MODULE__}
    end
  end

  defimpl Collectable do
    def into(original) do
      {original,
       fn
         bimultimap, {:cont, pair} -> BiMultiMap.put(bimultimap, pair)
         bimultimap, :done -> bimultimap
         _, :halt -> :ok
       end}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(bimultimap, opts) do
      concat(["BiMultiMap.new(", to_doc(BiMultiMap.to_list(bimultimap), opts), ")"])
    end
  end
end
