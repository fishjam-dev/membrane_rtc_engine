defimpl Enumerable, for: Heap do
  @doc """
  Returns the number of elements in a heap.

  ## Examples

      iex> 1..500
      ...>  |> Enum.into(Heap.new())
      ...>  |> Enum.count()
      500
  """
  @spec count(Heap.t()) :: {:ok, non_neg_integer}
  def count(heap) do
    {:ok, Heap.size(heap)}
  end

  @doc """
  Returns true if value is a contained in the heap.

  ## Examples

      iex> 1..500
      ...>   |> Enum.into(Heap.new())
      ...>   |> Enum.member?(250)
      true

      iex> 1..500
      ...>   |> Enum.into(Heap.new())
      ...>   |> Enum.member?(750)
      false
  """
  @spec member?(Heap.t(), term) :: {:ok, boolean}
  def member?(heap, value) do
    {:ok, Heap.member?(heap, value)}
  end

  @doc """
  Allows reduction to be applied to Heaps.

  ## Examples

      iex> 1..500
      ...>  |> Enum.shuffle()
      ...>  |> Enum.into(Heap.new())
      ...>  |> Enum.filter(&(Integer.mod(&1, 2) == 0))
      ...>  |> Enum.count()
      250
  """
  @spec reduce(Heap.t(), Enumerable.acc(), Enumerable.reducer()) :: Enumerable.result()
  def reduce(_, {:halt, acc}, _fun), do: {:halted, acc}
  def reduce(heap, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(heap, &1, fun)}

  def reduce(heap, {:cont, acc}, fun) do
    case Heap.root(heap) do
      nil ->
        {:done, acc}

      root ->
        heap = Heap.pop(heap)
        reduce(heap, fun.(root, acc), fun)
    end
  end

  @doc """
  There's no fast way to slice a Heap so we return `{:error, __MODULE__}` to
  allow Elixir to use the default algorithm.
  """
  @spec slice(Heap.t()) :: {:error, __MODULE__}
  def slice(_heap), do: {:error, __MODULE__}
end
