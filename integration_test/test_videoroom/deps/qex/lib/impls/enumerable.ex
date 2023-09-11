defimpl Enumerable, for: Qex do
  def count(%Qex{data: q}), do: {:ok, :queue.len(q)}

  def member?(%Qex{data: q}, item) do
    {:ok, :queue.member(item, q)}
  end

  def reduce(%Qex{data: q}, acc, fun) do
    Enumerable.List.reduce(:queue.to_list(q), acc, fun)
  end
  
  def slice(%Qex{}), do: {:error, __MODULE__}
end
