defmodule TestBrowser.Application do
  @moduledoc false

  use Application

  # in milliseconds
  @test_warmup 5000

  @impl true
  def start(_type, _args) do
    # Overhead for media server startup
    Process.sleep(@test_warmup)

    :ok = TestBrowser.PacketLoss.run_test()

    {:ok, self()}
  end
end
