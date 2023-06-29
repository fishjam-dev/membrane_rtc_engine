defmodule TestVideoroom.TestResultReceiver do
  @moduledoc false

  use GenServer
  require Logger

  @browser_count 3
  @shared_folder "shared"

  # in milliseconds
  @max_test_duration 400_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    Process.send_after(self(), :test_duration_exceeded, @max_test_duration)

    {:ok, %{results: %{}, received: 0}}
  end

  @impl true
  def handle_info({:stats, name, stats}, %{results: results} = state) do
    Logger.info("Received stats from #{inspect(name)}")

    results = Map.put(results, name, stats)
    received = Map.keys(results) |> length()
    if received == @browser_count, do: compare_results(results)

    {:noreply, %{state | results: results, received: received}}
  end

  @impl true
  def handle_info({:enable_packet_loss, name}, state) do
    Logger.info("#{inspect(name)} reports ready to enable packet loss")

    File.write!(Path.join(@shared_folder, "ENABLE_PACKET_LOSS"), <<>>)

    {:noreply, state}
  end

  @impl true
  def handle_info(:test_duration_exceeded, _state) do
    Logger.error("Test duration exceeded!")

    System.stop(1)
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
