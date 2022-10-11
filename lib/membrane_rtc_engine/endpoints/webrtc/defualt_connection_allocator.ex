defmodule Membrane.RTC.Engine.Endpoint.WebRTC.DefaultConnectionAllocator do
  @moduledoc """
  Implementation of `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator` that grants all allocations immediately.

  It might be useful for non-WebRTC Endpoints
  """
  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionProber.AllocationGrantedNotification

  @impl true
  def say_hello(_manager, _bandwidth, _track), do: :ok

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
end
