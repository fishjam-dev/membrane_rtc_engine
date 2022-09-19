# Logs

RTC Engine and its endpoints add following metadata to logs:
* rtc_engine - RTC Engine id passed to `Membrane.RTC.Engine.start_link/2` or `Membrane.RTC.Engine.start/2`
* webrtc_endpoint - WebRTC endpoint id passed as peer_id to `Membrane.RTC.Engine.add_endpoint/3`

Metadata has to be turned on in config files with

```elixir
config :logger, :console, metadata: [:rtc_engine, :webrtc_endpoint]
```

Additionaly, some endpoints might accept their own log metadata e.g.

```elixir
endpoint = %WebRTC{
  log_metadata: [some_additional_metadata: "some_additional_metadata"],
}
```