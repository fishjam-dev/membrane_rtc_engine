defmodule Membrane.RTC.Engine.Endpoint.HLS.DummyBandwidthManager do
  @moduledoc false
  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.BandwidthManager

  @impl true
  def register_track_receiver(_manager, _pid), do: :ok

  @impl true
  def update_bandwidth_estimation(_manager, _estimation), do: :ok

  @impl true
  def probe_sent(_manager), do: :ok

  @impl true
  def buffer_sent(_manager, _buffer), do: :ok
end
