# Logs

RTC Engine adds following metadata to logs:
* rtc_engine_id - RTC Engine id passed to `Membrane.RTC.Engine.start_link/2` or `Membrane.RTC.Engine.start/2`

Metadata has to be turned on in config files with

```elixir
config :logger, :console, metadata: [:rtc_engine_id]
```

Additionaly, some endpoints might accept their own log metadata e.g.

```elixir
endpoint = %WebRTC{
  log_metadata: [peer_id: peer.id],
}
```