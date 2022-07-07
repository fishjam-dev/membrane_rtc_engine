# Traces

RTC Engine uses [`membrane_opentelemetry`](https://github.com/membraneframework/membrane_opentelemetry) to generate OpenTelemetry traces and spans.
You can enable traces in your `config.exs`:
```elixir
config :membrane_opentelemetry, enabled: true
```
Besides this you need to put `:opentelemetry` config in `config/runtime.exs`, for example
```elixir
config :opentelemetry, :resource,
  service: [
    name: "membrane_rtc_engine",
    namespace: "membrane"
  ],
  processors: [
    otel_batch_processor: %{
      exporter: {:otel_exporter_stdout, []}
    }
  ]
```
Where:
  * `service` - name and namespace of the service. They are used for easier distinction from other services. You can read more [here](https://opentelemetry.io/docs/reference/specification/resource/semantic_conventions/#service)
  * `processors` - defines when spans should be exported. `otel_simple_procesor` flushes each span immediately after ending it while `otel_batch_processor` buffers several spans and flushes them in a batch. For more see [otel_simple_processor](https://hexdocs.pm/opentelemetry/otel_simple_processor.html) or [otel_batch_processor](https://hexdocs.pm/opentelemetry/otel_batch_processor.html)
  * `exporter` - defines where spans should be exported. otel_exporter_stdout prints spans to stdout but there are other exporters that can visualize traces in a human readable format like [Zipkin](https://github.com/open-telemetry/opentelemetry-erlang/tree/main/apps/opentelemetry_zipkin)

To see more about `:opentelemetry` config, you can go to the OpenTelemetry [documentation](https://opentelemetry.io/docs/instrumentation/erlang/getting-started/#initialization-and-configuration)

You can also go to [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom) for more examples.
