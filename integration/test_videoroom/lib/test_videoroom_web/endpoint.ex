defmodule TestVideoroomWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :test_videoroom

  @session_options [
    store: :cookie,
    key: "_test_videoroom_key",
    signing_salt: "ptQFHLMN"
  ]

  socket "/socket", TestVideoroomWeb.UserSocket,
    websocket: true,
    longpoll: false

  plug Plug.Static,
    at: "/",
    from: :test_videoroom,
    gzip: false,
    only: ~w(assets fonts images favicon.ico robots.txt)

  plug Plug.RequestId

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug TestVideoroomWeb.Router
end
