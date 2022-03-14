import Config

config :test_videoroom, TestVideoroomWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 4001],
  watchers: [
    # Start the esbuild watcher by calling Esbuild.install_and_run(:default, args)
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ],
  server: true
