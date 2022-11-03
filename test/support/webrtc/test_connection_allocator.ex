defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TestConnectionAllocator do
  @moduledoc false
  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator

  @impl true
  def start_link(), do: {:ok, self()}

  @impl true
  def register_track_receiver(allocator, bandwidth, track) do
    send(allocator, {:register_tr, self(), bandwidth, track})
    :ok
  end

  @impl true
  def request_allocation(allocator, allocation) do
    send(allocator, {:request_allocation, self(), allocation})
    :ok
  end

  @impl true
  def update_bandwidth_estimation(_allocator, _estimation) do
    raise "Unimplemented!"
  end

  @impl true
  def buffer_sent(_allocator, _buffer) do
    :ok
  end

  @impl true
  def probe_sent(_allocator) do
    :ok
  end
end
