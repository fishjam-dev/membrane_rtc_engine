import Config

config :phoenix, :json_library, Jason
config :opentelemetry, traces_exporter: :none

config :esbuild,
  version: "0.12.15",
  default: [
    args:
      ~w(src/index.ts --bundle --target=es2020 --outfile=../priv/static/assets/js/app.js --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :membrane_videoroom_demo, VideoRoomWeb.Endpoint, pubsub_server: VideoRoom.PubSub

config :membrane_videoroom_demo, version: System.get_env("VERSION", "unknown")

config :logger, :console, metadata: [:room, :peer]

import_config("#{config_env()}.exs")
