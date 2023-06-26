defmodule TestBrowserTest do
  use ExUnit.Case

  # in milliseconds
  @test_warmup 5000
  @max_test_duration 400_000

  @tag timeout: @max_test_duration
  test "run browser for packet loss test" do
    # Overhead for media server startup
    Process.sleep(@test_warmup)

    :ok = TestBrowser.PacketLoss.run_test()
  end
end
