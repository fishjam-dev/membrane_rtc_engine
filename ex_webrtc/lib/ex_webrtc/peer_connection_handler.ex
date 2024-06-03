defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandler do
  @moduledoc false
  use Membrane.Endpoint

  alias ExWebRTC.PeerConnection

  @ice_servers [
    %{urls: "stun:stun.l.google.com:19302"}
  ]

  @impl true
  def handle_init(_ctx, _opts) do
    {:ok, pc} = PeerConnection.start_link(ice_servers: @ice_servers)

    # {:ok, _transceiver} = PeerConnection.add_transceiver(pc, :video, direction: :recvonly)
    # {:ok, _transceiver} = PeerConnection.add_transceiver(pc, :audio, direction: :recvonly)

    # {:ok, offer} = PeerConnection.create_offer()

    {[], %{pc: pc}}
  end

  @impl true
  def handle_pad_added(_pad, _ctx, state) do

    {[], state}
  end

  @impl true
  def handle_buffer(_pad, _buffer, _ctx, state) do

    {[], state}
  end
end
