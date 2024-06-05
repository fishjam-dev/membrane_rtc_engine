defmodule WebRTCRoom.PeerHandler do
  require Logger

  alias WebRTCRoom.EngineHandler

  @behaviour WebSock

  @impl true
  def init(_) do
    Logger.info("WebSocket connection #{inspect(self())} established")
    EngineHandler.add_peer()

    {:ok, %{}}
  end

  @impl true
  def handle_in({msg, [opcode: :text]}, state) do
    case Jason.decode!(msg) do
      %{"type" => "mediaEvent", "data" => event} -> EngineHandler.forward_media_event(event)
      other -> Logger.warning("Received unexpected WebSocket message: #{inspect(other)}")
    end

    {:ok, state}
  end

  @impl true
  def handle_info({:media_event, event}, state) do
    msg = Jason.encode!(%{"type" => "mediaEvent", "data" => event})
    {:push, {:text, msg}, state}
  end

  @impl true
  def terminate(reason, _state) do
    Logger.warning(
      "WebSocket connection #{inspect(self())} terminated with reason: #{inspect(reason)}"
    )
  end
end
