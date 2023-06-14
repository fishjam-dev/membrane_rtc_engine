defmodule TestVideoroom.TestResultReceiver do
  @moduledoc false

  use GenServer
  require Logger

  @browser_count 3
  @shared_folder "shared"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:ok, %{results: %{}, received: 0}}
  end

  @impl true
  def handle_info({:stats, name, stats}, %{results: results, received: received} = state) do
    Logger.info("Received stats from #{inspect(name)}")

    state = %{state | results: Map.put(results, name, stats), received: received + 1}
    if state.received == @browser_count, do: compare_results(state.results)

    {:noreply, state}
  end

  @impl true
  def handle_info({:enable_packet_loss, name}, state) do
    Logger.info("#{inspect(name)} reports ready to enable packet loss")

    File.write!(Path.join(@shared_folder, "ENABLE_PACKET_LOSS"), <<>>)

    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.warn("Received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  defp compare_results(results) do
    # TODO: Determine whether test passed or not
    IO.inspect(results, label: :results_are)
    if true, do: System.stop(0)
    raise("TEST FAILED")
  end
end
