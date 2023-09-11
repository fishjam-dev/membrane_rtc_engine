# Membrane OpenTelemetry

[![Hex.pm](https://img.shields.io/hexpm/v/membrane_opentelemetry.svg)](https://hex.pm/packages/membrane_opentelemetry)
[![API Docs](https://img.shields.io/badge/api-docs-yellow.svg?style=flat)](https://hexdocs.pm/membrane_opentelemetry)
[![CircleCI](https://circleci.com/gh/membraneframework/membrane_opentelemetry.svg?style=svg)](https://circleci.com/gh/membraneframework/membrane_opentelemetry)

This repository contains Membrane wrappers of OpenTelemetry functions, designed to make introducing OpenTelemetry to Membrane Plugins easier.

It is part of [Membrane Multimedia Framework](https://membraneframework.org).

## Installation

The package can be installed by adding `membrane_opentelemetry` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:membrane_opentelemetry, "~> 0.1.0"}
  ]
end
```

## Usage

Firstly, you have to turn on `Membrane.OpenTelemetry` in your config files. To do this, add 
```elixir
config :membrane_opentelemetry, enabled: true
```
to your `config/config.exs` file. Beyond that, you have to turn on OpenTelemetry itself. You can do this by adding 
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
to your `config/runtime.exs` file. In this case, you will use exporter, that will put tracing data on stdout, but you can use Zipkin or Honeycomb exporters for OpenTelemetry as well (take a look on (membrane_videoroom)[https://github.com/membraneframework/membrane_videoroom] (config/runtime.exs)[https://github.com/membraneframework/membrane_videoroom/blob/master/config/runtime.exs], if you want an example).

To start your fist span, you have to call 
```elixir
Membrane.OpenTelemetry.start_span("root_span")
```
You have just starterd new span, that id is `"root_span"`. Span id is used to identify span within one process. You cannot have two different spans with this same id in one process.

Then, if you want to start a span, that will be a child of a `root_span`, call
```elixir
Membrane.OpenTelemetry.start_span("child_span", parent_id: "root_span")
```

You can also specify span parent, by passing its `span_ctx`. It is useful, in the case, when you want to start a span, that will be a child of a span, that comes from another process and, for example, was passed to your process by message.
```elixir
Membrane.OpenTelemetry.start_span("another_child_span", parent_span: parent_span_ctx) 
```

To get the span context, call 
```elixir
root_span_ctx = Membrane.OpenTelemetry.get_span("root_span")
```
in the process, that started the `root_span`.

Span name is a string, that probably will be used to identify your span in the visualization tool, like Zipkin or Honeycomb. By default, span name is equal to id, but you can override it by passing `:name` option to `Membrane.OpenTelemetry.start_span/2`. 
```elixir
Membrane.OpenTelemetry.start_span("grandchild_span", parent_id: "child_span", name: "grandchild_span_name") 
```

To end a span, just call
```elixir
Membrane.OpenTelemetry.end_span("another_child_span")
```

You can also set span attributes or add an event to it
```elixir
Membrane.OpenTelemetry.set_attributes("root_span", children_number: 2, is_root_span: true)
Membrane.OpenTelemetry.add_event("child_span", :example_event_name, event_attribute_key: "event attribute value")
Membrane.OpenTelemetry.set_attribute("child_span", :has_children, true)
```

## Copyright and License

Copyright 2022, [Software Mansion](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_opentelemetry)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/?utm_source=git&utm_medium=readme&utm_campaign=membrane_opentelemetry)

Licensed under the [Apache License, Version 2.0](LICENSE)
