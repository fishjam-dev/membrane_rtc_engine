# OpenTelemetry

RTC Engine uses [`membrane_opentelemetry`](https://github.com/membraneframework/membrane_opentelemetry) to generate OpenTelemetry traces and spans.
You can enable traces in your `config.exs`:
```elixir
config :membrane_opentelemetry, enabled: true
```
in `config/config.exs` end put `:opentelemetry` config in `config/runtime.exs`, for example
```elixir
config :opentelemetry, :resource,
  service: [
    name: "membrane",
    namespace: "membrane"
  ],
  tracer: :otel_tracer_default,
  processors: [
    otel_batch_processor: %{
      exporter: {:otel_exporter_stdout, []}
    }
  ]
```
To see more about `:opentelemetry` config, you can go to the OpenTelemetry [documentacion](https://opentelemetry.io/docs/instrumentation/erlang/getting-started/#initialization-and-configuration)

You can also go to [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom) for more examples.