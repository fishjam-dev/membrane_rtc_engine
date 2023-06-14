defmodule TestBrowser.Utils do
  @moduledoc false

  def receive_stats(recipient, acc \\ %{}) do
    receive do
      {browser_id, :end} ->
        send(recipient, {:stats, browser_id, acc})
        :ok

      {browser_id, tag, data} ->
        acc
        |> then(fn acc ->
          default_map = %{browser_id => [data]}

          Map.update(
            acc,
            tag,
            default_map,
            fn tag_map -> Map.update(tag_map, browser_id, [data], &(&1 ++ [data])) end
          )
        end)
        |> then(&receive_stats(recipient, &1))
    end
  end
end
