defmodule Membrane.RTC.Utils do
  @moduledoc false
  use OpenTelemetryDecorator
  require OpenTelemetry.Tracer, as: Tracer

  @spec find_child(ctx :: any(), pattern :: any()) :: Membrane.ChildEntry.t()
  defmacro find_child(ctx, pattern: pattern) do
    quote do
      unquote(ctx).children |> Map.keys() |> Enum.find(&match?(unquote(pattern), &1))
    end
  end

  @spec reduce_children(ctx :: Membrane.Pipeline.CallbackContext.t(), acc :: any(), fun :: fun()) ::
          any()
  def reduce_children(ctx, acc, fun) do
    ctx.children |> Map.keys() |> Enum.reduce(acc, fun)
  end

  @spec flat_map_children(ctx :: Membrane.Pipeline.CallbackContext.t(), fun :: fun()) :: [any()]
  def flat_map_children(ctx, fun) do
    ctx.children |> Map.keys() |> Enum.flat_map(fun)
  end

  @spec forward(
          child_name :: any(),
          msg :: any(),
          ctx :: Membrane.Pipeline.CallbackContext.t()
        ) :: [Membrane.Element.Action.notify_t()]
  def forward(child_name, msg, ctx) do
    child = find_child(ctx, pattern: ^child_name)

    if child do
      [forward: {child_name, msg}]
    else
      []
    end
  end

  @spec create_otel_context(name :: String.t(), metadata :: KeywordList.t()) :: any()
  def create_otel_context(name, metadata \\ []) do
    metadata =
      [
        {:"library.language", :erlang},
        {:"library.name", :membrane_rtc_engine},
        {:"library.version", "semver:#{Application.spec(:membrane_rtc_engine, :vsn)}"}
      ] ++ metadata

    root_span = Tracer.start_span(name)
    parent_ctx = Tracer.set_current_span(root_span)
    otel_ctx = OpenTelemetry.Ctx.attach(parent_ctx)
    OpenTelemetry.Span.set_attributes(root_span, metadata)
    OpenTelemetry.Span.end_span(root_span)
    OpenTelemetry.Ctx.attach(otel_ctx)

    [otel_ctx]
  end
end
