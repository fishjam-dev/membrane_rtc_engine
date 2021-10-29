# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
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

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :info],
    # Silence irrelevant warnings caused by resending handshake events
    [module: Membrane.SRTP.Encryptor, function: "handle_event/4", level_lower_than: :error]
  ]

config :logger, level: :info




# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# config :ex_libnice, impl: NIF


# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
