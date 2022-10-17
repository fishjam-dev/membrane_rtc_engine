defmodule Membrane.RTC.Engine.Endpoint.WebRTC.NoOpConnectionAllocator do
  @moduledoc """
  Implementation of `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator` that does nothing.

  It might be useful for non-WebRTC Endpoints
  """
  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator

  @impl true
  def register_track_receiver(_manager, _pid), do: :ok

  @impl true
  def update_bandwidth_estimation(_manager, _estimation), do: :ok

  @impl true
  def probe_sent(_manager), do: :ok

  @impl true
  def buffer_sent(_manager, _buffer), do: :ok
end
