defimpl Collectable, for: Qex do
  def into(%Qex{} = qex) do
    {qex, &push/2}
  end

  defp push(q, {:cont, item}), do: Qex.push(q, item)
  defp push(q, :done), do: q
  defp push(_q, :halt), do: :ok
end
