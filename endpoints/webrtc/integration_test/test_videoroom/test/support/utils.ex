defmodule TestVideoroom.Integration.Utils do
  @moduledoc false

  def receive_stats(mustangs_number, pid, acc \\ %{}) do
    if mustangs_number > 0 do
      receive do
        {_browser_id, :end} ->
          receive_stats(mustangs_number - 1, pid, acc)

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
          |> then(&receive_stats(mustangs_number, pid, &1))
      end
    else
      send(pid, acc)
    end
  end
end
