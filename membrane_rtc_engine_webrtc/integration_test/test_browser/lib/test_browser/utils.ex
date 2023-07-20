defmodule TestBrowser.Utils do
  @moduledoc false

  # in milliseconds
  @send_interval 1000

  def receive_stats(recipient, acc \\ %{}) do
    receive do
      {browser_id, :end} ->
        # HACK: If packet loss emulation is enabled, this message may get dropped - so, we keep
        # sending it over and over again...
        # TODO: Remove once the netem command is fully synchronised with the browsers and server
        send_loop(recipient, {:stats, browser_id, acc})
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

  defp send_loop(recipient, msg) do
    send(recipient, msg)

    # Can't use `Process.send_after/3` as it doesn't work with remote nodes
    Process.sleep(@send_interval)

    send_loop(recipient, msg)
  end
end
