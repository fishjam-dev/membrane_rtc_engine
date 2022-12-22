import Config

config :membrane_rtc_engine,
  connection_prober_implementation: Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionProber

config :membrane_telemetry_metrics, :enabled, true

import_config "#{config_env()}.exs"
