defmodule TestVideoroom.Integration.ResultReceiver do
  @moduledoc false

  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(args) do
    state = %{
      browser_count: Keyword.fetch!(args, :browser_count),
      parent: Keyword.fetch!(args, :parent),
      results: %{},
      received: 0
    }

    {:ok, state}
  end

  @impl true
  def handle_info({:stats, name, stats}, %{results: results} = state) do
    Logger.info("Received stats from #{inspect(name)}")

    results = Map.put(results, name, stats)
    received = Map.keys(results) |> length()

    if received < state.browser_count do
      {:noreply, %{state | results: results, received: received}}
    else
      Logger.info("All #{inspect(state.browser_count)} stats received. Result receiver exiting")
      send(state.parent, {:results, results})

      {:stop, :normal, state}
    end
  end

  @impl true
  def handle_info(msg, state) do
    send(state.parent, msg)

    {:noreply, state}
  end
end
