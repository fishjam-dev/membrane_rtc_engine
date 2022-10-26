import Config

config :membrane_rtc_engine,
  connection_prober_implementation: Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionProber

import_config "#{config_env()}.exs"
