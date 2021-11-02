import Config

config :test_videoroom, TestVideoroomWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4001],
  watchers: [
    # Start the esbuild watcher by calling Esbuild.install_and_run(:default, args)
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ],
  server: true

config :wallaby, :driver, Wallaby.Selenium

config :wallaby, :hackney_options, timeout: 10_000, recv_timeout: 10_000
