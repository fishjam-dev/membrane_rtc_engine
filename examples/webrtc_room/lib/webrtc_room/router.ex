defmodule WebRTCRoom.Router do
  use Plug.Router

  plug(Plug.Logger)
  plug(Plug.Static, at: "/", from: :webrtc_room)
  plug(:match)
  plug(:dispatch)

  get "/ws" do
    WebSockAdapter.upgrade(conn, WebRTCRoom.PeerHandler, %{}, [])
  end

  match _ do
    send_resp(conn, 404, "not found")
  end
end
