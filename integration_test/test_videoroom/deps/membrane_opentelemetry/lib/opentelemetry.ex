defmodule Membrane.OpenTelemetry do
  @moduledoc """
  Defines macros for operations on (OpenTelemetry)[https://hexdocs.pm/opentelemetry_api] traces, spans and events.
  Provided macros expand to appropriate calls to (OpenTelemetry)[https://hexdocs.pm/opentelemetry_api] functions or to nothing, depending on config values.
  Purpose of this module, is to provide simple way of introducing OpenTelemetry into Membrane Elements, Bins and Piplines.
  """

  require OpenTelemetry
  require OpenTelemetry.Tracer
  require OpenTelemetry.Ctx

  @enabled Application.compile_env(:membrane_opentelemetry, :enabled, false)

  @type span_id :: String.t()

  @doc """
  Starts a new span.
  Returns context of newly created span.
  First argument is span id. Span id is used to identify span and must be unique within one process.

  Options:
  - `parent_span` - a span_ctx of a span, that will be the parent of the created span
  - `parent_id` - an id of a span, that will be the parent of the created span. Shouldn't be used with `parent_span` option. Available only for spans, that were created or stored in the same process in which we call this macro.
  - `name` - name of a span. If not provided, by default span name will be span id
  """
  defmacro start_span(id, opts \\ []) do
    if enabled(),
      do: do_start_span(id, opts),
      else: void([id, opts])
  end

  @doc """
  Ends a span.
  """
  defmacro end_span(id) do
    if enabled(),
      do: do_end_span(id),
      else: void([id])
  end

  @doc """
  Sets an attribute value in a span with a specific id.
  """
  defmacro set_attribute(id, key, value) do
    if enabled(),
      do: do_set_attribute(id, key, value),
      else: void([id, key, value])
  end

  @doc """
  Sets attributes in a span with specific id.
  """
  defmacro set_attributes(id, attributes) do
    if enabled(),
      do: do_set_attributes(id, attributes),
      else: void([id, attributes])
  end

  @doc """
  Adds an event to a span with a specific id.
  """
  defmacro add_event(id, event, attributes \\ []) do
    if enabled(),
      do: do_add_event(id, event, attributes),
      else: void([id, event, attributes])
  end

  @doc """
  Adds events to a span with a specific id.
  """
  defmacro add_events(id, events) do
    if enabled(),
      do: do_add_events(id, events),
      else: void([id, events])
  end

  @doc """
  Creates new `otel_ctx`. See docs for `OpenTelemetry.Ctx.new/1`.
  """
  defmacro new_ctx() do
    if enabled() do
      quote do
        OpenTelemetry.Ctx.new()
      end
    end
  end

  @doc """
  Attaches `otel_ctx`. See docs for `OpenTelemetry.Ctx.attach/1`.
  """
  defmacro attach(ctx) do
    if enabled() do
      quote do
        OpenTelemetry.Ctx.attach(unquote(ctx))
      end
    else
      void(ctx)
    end
  end

  @spec get_span(Membrane.OpenTelemetry.span_id()) :: :opentelemetry.span_ctx() | nil
  defdelegate get_span(id), to: __MODULE__.ETSUtils

  defp enabled(), do: @enabled

  defp do_start_span(id, opts) do
    quote do
      require OpenTelemetry.Tracer

      unquote(__MODULE__).Monitor.ensure_monitor_started()

      opts_map = unquote(opts) |> Map.new()
      old_current_span = OpenTelemetry.Tracer.current_span_ctx()

      case opts_map do
        %{parent_id: parent_id} ->
          parent_span = unquote(__MODULE__).ETSUtils.get_span(parent_id)
          OpenTelemetry.Tracer.set_current_span(parent_span)

        %{parent_span: parent_span} ->
          OpenTelemetry.Tracer.set_current_span(parent_span)

        _else ->
          :ok
      end

      links =
        case opts_map do
          %{linked_spans: spans} when is_list(spans) -> spans
          _else -> []
        end
        |> Enum.map(&OpenTelemetry.link/1)

      span_name =
        case opts_map do
          %{name: name} -> name
          _else -> unquote(id)
        end

      new_span = OpenTelemetry.Tracer.start_span(span_name, %{links: links})
      unquote(__MODULE__).ETSUtils.store_span(unquote(id), new_span)

      if old_current_span,
        do: OpenTelemetry.Tracer.set_current_span(old_current_span)

      new_span
    end
  end

  defp do_end_span(id) do
    quote do
      with span when span != nil <- unquote(__MODULE__).ETSUtils.pop_span(unquote(id)) do
        OpenTelemetry.Span.end_span(span)
      end
    end
  end

  defp do_set_attribute(id, key, value) do
    quote do
      unquote(id)
      |> unquote(__MODULE__).ETSUtils.get_span()
      |> OpenTelemetry.Span.set_attribute(unquote(key), unquote(value))
    end
  end

  defp do_set_attributes(id, attributes) do
    quote do
      unquote(id)
      |> unquote(__MODULE__).ETSUtils.get_span()
      |> OpenTelemetry.Span.set_attributes(unquote(attributes))
    end
  end

  defp do_add_event(id, event, attributes) do
    quote do
      unquote(id)
      |> unquote(__MODULE__).ETSUtils.get_span()
      |> OpenTelemetry.Span.add_event(unquote(event), unquote(attributes))
    end
  end

  defp do_add_events(id, events) do
    quote do
      unquote(id)
      |> unquote(__MODULE__).ETSUtils.get_span()
      |> OpenTelemetry.Span.add_events(unquote(events))
    end
  end

  defp void(values) do
    quote do
      fn ->
        _unused = unquote(values)
      end
    end
  end
end
