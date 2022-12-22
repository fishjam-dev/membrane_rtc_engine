defmodule Membrane.RTC.Engine.Endpoint.WebRTC.NoOpConnectionAllocator do
  @moduledoc """
  Implementation of `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator` that grants all allocations immediately.

  It might be useful for non-WebRTC Endpoints
  """
  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.AllocationGrantedNotification

  @impl true
  def create(), do: {:ok, nil}

  @impl true
  def destroy(nil), do: :ok

  @impl true
  def register_track_receiver(_manager, _bandwidth, _track, _options \\ []), do: :ok

  @impl true
  def update_bandwidth_estimation(_manager, _estimation), do: :ok

  @impl true
  def probe_sent(_manager), do: :ok

  @impl true
  def buffer_sent(_manager, _buffer), do: :ok

  @impl true
  def request_allocation(_manager, requested_allocation) do
    send(self(), %AllocationGrantedNotification{allocation: requested_allocation})
    :ok
  end

  @impl true
  def release_bandwidth(_manager, _number) do
    :ok
  end

  @impl true
  def set_negotiability_status(_manager, _value), do: :ok
end
