# Metrics

RTC Engine uses [`membrane_opentelemetry`](https://github.com/membraneframework/membrane_opentelemetry) to create OpenTelemetry traces and spans describing flow of operations in it.
To enable generating spans, you have to put line 
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

You can also go to [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom) for more examples.