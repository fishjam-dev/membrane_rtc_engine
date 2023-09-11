defimpl Inspect, for: Heap do
  import Inspect.Algebra

  @moduledoc """
  Implements `Inspect` for `Heap`.
  """

  @doc """
  Format the heap nicely for inspection.

  ## Examples

      iex> 1..10
      ...>   |> Enum.into(Heap.max())
      ...>   |> inspect
      "#Heap<[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]>"
  """
  @spec inspect(Heap.t(), Inspect.Opts.t()) :: Inspect.Algebra.t()
  def inspect(heap, opts) do
    concat(["#Heap<", to_doc(Enum.to_list(heap), opts), ">"])
  end
end
