import Config

config :logger, level: :info

config :test_videoroom, TestVideoroomWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 4001],
  server: true


config :membrane_rtc_engine,
  connection_prober_implementation: Membrane.RTC.Engine.Endpoint.WebRTC.NoOpConnectionAllocator
