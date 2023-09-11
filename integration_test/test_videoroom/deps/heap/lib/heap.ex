defmodule Heap do
  defstruct data: nil, size: 0, comparator: nil

  @moduledoc """
  A heap is a special tree data structure. Good for sorting and other magic.

  See also: [Heap (data structure) on Wikipedia](https://en.wikipedia.org/wiki/Heap_(data_structure)).
  """

  @type t() :: %Heap{
          data: tuple() | nil,
          size: non_neg_integer(),
          comparator: :> | :< | (any(), any() -> boolean())
        }

  @doc """
  Create an empty min `Heap`.

  A min heap is a heap tree which always has the smallest value at the root.

  ## Examples

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.min())
      ...>   |> Heap.root()
      1
  """
  @spec min() :: t
  def min, do: Heap.new(:<)

  @doc """
  Create an empty max `Heap`.

  A max heap is a heap tree which always has the largest value at the root.

  ## Examples

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.max())
      ...>   |> Heap.root()
      10
  """
  @spec max() :: t
  def max, do: Heap.new(:>)

  @doc """
  Create an empty `Heap` with the default comparator (`<`).

  Defaults to `>`.

  ## Examples

      iex> Heap.new()
      ...>   |> Heap.comparator()
      :<
  """
  @spec new() :: t
  def new, do: %Heap{comparator: :<}

  @doc """
  Create an empty heap with a specific comparator.

  Provide a `comparator` option, which can be `:<`, `:>` to indicate
  that the `Heap` should use Elixir's normal `<` or `>` comparison functions
  or a custom comparator function.

    ## Examples

        iex> Heap.new(:<)
        ...>   |> Heap.comparator()
        :<

  If given a function it should compare two arguments, and return `true` if
  the first argument precedes the second one.

    ## Examples

        iex> 1..10
        ...>   |> Enum.shuffle()
        ...>   |> Enum.into(Heap.new(&(&1 > &2)))
        ...>   |> Enum.to_list()
        [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

        iex> Heap.new(&(Date.compare(elem(&1, 0), elem(&2, 0)) == :gt))
        ...>   |> Heap.push({~D[2017-11-20], :jam})
        ...>   |> Heap.push({~D[2017-11-21], :milk})
        ...>   |> Heap.push({~D[2017-10-21], :bread})
        ...>   |> Heap.push({~D[2017-10-20], :eggs})
        ...>   |> Enum.map(fn {_, what} -> what end)
        [:milk, :jam, :bread, :eggs]
  """
  @spec new(:> | :<) :: t
  def new(:>), do: %Heap{comparator: :>}
  def new(:<), do: %Heap{comparator: :<}
  @spec new((any, any -> boolean)) :: t
  def new(fun) when is_function(fun, 2), do: %Heap{comparator: fun}

  @doc """
  Test if `heap` is empty.

  ## Examples

      iex> Heap.new()
      ...>   |> Heap.empty?()
      true

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.empty?()
      false
  """
  @spec empty?(t) :: boolean()
  def empty?(%Heap{data: nil, size: 0}), do: true
  def empty?(%Heap{}), do: false

  @doc """
  Test if the `heap` contains the element `value`.

  ## Examples

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.member?(11)
      false

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.member?(7)
      true
  """
  @spec member?(t, any()) :: boolean()
  def member?(%Heap{} = heap, value) do
    root = Heap.root(heap)
    heap = Heap.pop(heap)
    has_member?(heap, root, value)
  end

  @doc """
  Push a new `value` into `heap`.

  ## Examples

      iex> Heap.new()
      ...>   |> Heap.push(13)
      ...>   |> Heap.root()
      13
  """
  @spec push(t, any()) :: t
  def push(%Heap{data: h, size: n, comparator: d}, value),
    do: %Heap{data: meld(h, {value, []}, d), size: n + 1, comparator: d}

  @doc """
  Pop the root element off `heap` and discard it.

  ## Examples

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.pop()
      ...>   |> Heap.root()
      2
  """
  @spec pop(t) :: t | nil
  def pop(%Heap{data: nil, size: 0} = _heap), do: nil

  def pop(%Heap{data: {_, q}, size: n, comparator: d} = _heap),
    do: %Heap{data: pair(q, d), size: n - 1, comparator: d}

  @doc """
  Return the element at the root of `heap`.

  ## Examples

      iex> Heap.new()
      ...>   |> Heap.root()
      nil

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.root()
      1
  """
  @spec root(t) :: any()
  def root(%Heap{data: {v, _}} = _heap), do: v
  def root(%Heap{data: nil, size: 0} = _heap), do: nil

  @doc """
  Return the number of elements in `heap`.

  ## Examples

      iex> 1..10
      ...>   |> Enum.shuffle()
      ...>   |> Enum.into(Heap.new())
      ...>   |> Heap.size()
      10
  """
  @spec size(t) :: non_neg_integer()
  def size(%Heap{size: n}), do: n

  @doc """
  Return the comparator `heap` is using for insert comparisons.

  ## Examples

      iex> Heap.new(:<)
      ...>   |> Heap.comparator()
      :<
  """
  @spec comparator(t) :: :< | :>
  def comparator(%Heap{comparator: d}), do: d

  @doc """
  Return the root element and the rest of the heap in one operation.

  ## Examples

      iex> heap = 1..10 |> Enum.into(Heap.min())
      ...> rest = Heap.pop(heap)
      ...> {1, rest} == Heap.split(heap)
      true
  """
  @spec split(t) :: {any, t}
  def split(%Heap{} = heap), do: {Heap.root(heap), Heap.pop(heap)}

  defp meld(nil, queue, _), do: queue
  defp meld(queue, nil, _), do: queue

  defp meld({k0, l0}, {k1, _} = r, :<) when k0 < k1, do: {k0, [r | l0]}
  defp meld({_, _} = l, {k1, r0}, :<), do: {k1, [l | r0]}

  defp meld({k0, l0}, {k1, _} = r, :>) when k0 > k1, do: {k0, [r | l0]}
  defp meld({_, _} = l, {k1, r0}, :>), do: {k1, [l | r0]}

  defp meld({k0, l0} = l, {k1, r0} = r, fun) when is_function(fun, 2) do
    case fun.(k0, k1) do
      true -> {k0, [r | l0]}
      false -> {k1, [l | r0]}
      err -> raise("Comparator should return boolean, but returned '#{err}'.")
    end
  end

  defp pair([], _), do: nil
  defp pair([q], _), do: q

  defp pair([q0, q1 | q], d) do
    q2 = meld(q0, q1, d)
    meld(q2, pair(q, d), d)
  end

  defp has_member?(_, previous, compare) when previous == compare, do: true
  defp has_member?(nil, _, _), do: false

  defp has_member?(heap, _, compare) do
    {previous, heap} = Heap.split(heap)
    has_member?(heap, previous, compare)
  end
end
