import Config

config :test_videoroom,
  ecto_repos: [TestVideoroom.Repo]

# Configures the endpoint
config :test_videoroom, TestVideoroomWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: TestVideoroomWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: TestVideoroom.PubSub

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.12.18",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2016 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :info],
    # Silence irrelevant warnings caused by resending handshake events
    [module: Membrane.SRTP.Encryptor, function: "handle_event/4", level_lower_than: :error],
    [module: Membrane.ICE, level_lower_than: :error]
  ]

config :logger, level: :info

config :phoenix, :json_library, Jason

import_config "#{config_env()}.exs"
