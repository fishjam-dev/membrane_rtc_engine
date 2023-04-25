defmodule Membrane.RTC.Utils do
  @moduledoc false

  require OpenTelemetry.Tracer, as: Tracer
  require Membrane.TelemetryMetrics

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTP.PayloadFormatResolver

  @rtp_packet_arrival_event [Membrane.RTC.Engine, :RTP, :packet, :arrival]
  @variant_switched_event [Membrane.RTC.Engine, :RTP, :variant, :switched]
  @bandwidth_event [Membrane.RTC.Engine, :peer, :bandwidth]

  # This is workaround to make dialyzer happy.
  # In other case we would have to specify all possible CallbackContext types here.
  # Maybe membrane_core should have something like
  # @type Membrane.Pipeline.CallbackContxt.t() ::  CallbackContext.Notification.t()
  #  | CallbackContext.Other.t()
  #  | CallbackContext.PlaybackChange.t()
  #  | etc.
  # to make it easier to reference CallbackContext types.
  @type ctx :: any()

  defmacro find_child(ctx, pattern: pattern) do
    quote do
      require Membrane.Child, as: Child

      unquote(ctx).children
      |> Enum.find(fn
        {unquote(pattern), _child_data} -> true
        _child_entry -> false
      end)
      |> case do
        {child_ref, _child_data} -> child_ref
        nil -> nil
      end
    end
  end

  defmacro filter_children(ctx, pattern: pattern) do
    quote do
      require Membrane.Child, as: Child

      unquote(ctx).children
      |> Map.keys()
      |> Enum.filter(fn child_name ->
        match?(Child.ref(unquote(pattern)), child_name) or
          match?(Child.ref(unquote(pattern), group: _group), child_name)
      end)
    end
  end

  @spec reduce_children(ctx :: ctx(), acc :: any(), fun :: fun()) ::
          any()
  def reduce_children(ctx, acc, fun) do
    ctx.children |> Map.keys() |> Enum.reduce(acc, fun)
  end

  @spec flat_map_children(ctx :: ctx(), fun :: fun()) :: [any()]
  def flat_map_children(ctx, fun) do
    ctx.children |> Map.keys() |> Enum.flat_map(fun)
  end

  @spec forward(
          child_name :: any(),
          msg :: any(),
          ctx :: ctx()
        ) :: [Membrane.Pipeline.Action.notify_child_t()]
  def forward(child_name, msg, ctx) do
    child_ref = find_child(ctx, pattern: ^child_name)

    if child_ref do
      [notify_child: {child_ref, msg}]
    else
      []
    end
  end

  @spec send_if_not_nil(pid :: pid() | nil, msg :: any()) :: any()
  def send_if_not_nil(pid, msg) do
    if pid != nil do
      send(pid, msg)
    end
  end

  @spec create_otel_context(name :: String.t(), metadata :: [{atom(), any()}]) :: any()
  @dialyzer {:nowarn_function, create_otel_context: 1, create_otel_context: 2}
  def create_otel_context(name, metadata \\ []) do
    metadata =
      [
        {:"library.language", :erlang},
        {:"library.name", :membrane_rtc_engine},
        {:"library.version", "server:#{Application.spec(:membrane_rtc_engine, :vsn)}"}
      ] ++ metadata

    root_span = Tracer.start_span(name)
    parent_ctx = Tracer.set_current_span(root_span)
    otel_ctx = OpenTelemetry.Ctx.attach(parent_ctx)
    OpenTelemetry.Span.set_attributes(root_span, metadata)
    OpenTelemetry.Span.end_span(root_span)
    OpenTelemetry.Ctx.attach(otel_ctx)

    [otel_ctx]
  end

  @spec generate_turn_credentials(binary(), binary()) :: {binary(), binary()}
  def generate_turn_credentials(name, secret) do
    duration =
      DateTime.utc_now()
      |> DateTime.to_unix()
      |> tap(fn unix_timestamp -> unix_timestamp + 24 * 3600 end)

    username = "#{duration}:#{name}"

    password =
      :crypto.mac(:hmac, :sha, secret, username)
      |> Base.encode64()

    {username, password}
  end

  @spec telemetry_register(Membrane.TelemetryMetrics.label()) :: :ok
  def telemetry_register(telemetry_label) do
    Membrane.TelemetryMetrics.register(@rtp_packet_arrival_event, telemetry_label)
    :ok
  end

  @spec register_bandwidth_event(Membrane.TelemetryMetrics.label()) :: :ok
  def register_bandwidth_event(telemetry_label) do
    Membrane.TelemetryMetrics.register(@bandwidth_event, telemetry_label)
    :ok
  end

  @spec register_variant_switched_event(Membrane.TelemetryMetrics.label()) :: :ok
  def register_variant_switched_event(telemetry_label) do
    Membrane.TelemetryMetrics.register(@variant_switched_event, telemetry_label)
    :ok
  end

  @spec emit_packet_arrival_event(
          binary(),
          :VP8 | :H264 | :OPUS,
          Membrane.TelemetryMetrics.label()
        ) :: :ok
  def emit_packet_arrival_event(payload, codec, telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @rtp_packet_arrival_event,
      packet_measurements(payload, codec),
      %{},
      telemetry_label
    )

    :ok
  end

  @spec emit_bandwidth_event(float(), Membrane.TelemetryMetrics.label()) :: :ok
  def emit_bandwidth_event(bandwidth, telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @bandwidth_event,
      %{bandwidth: bandwidth},
      %{},
      telemetry_label
    )

    :ok
  end

  @spec emit_variant_switched_event(
          Track.variant(),
          TrackReceiver.variant_switch_reason(),
          Membrane.TelemetryMetrics.label()
        ) :: :ok
  def emit_variant_switched_event(variant, reason, telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @variant_switched_event,
      %{variant: variant, reason: reason},
      %{},
      telemetry_label
    )

    :ok
  end

  defp packet_measurements(payload, codec) do
    measurements =
      case PayloadFormatResolver.keyframe_detector(codec) do
        {:ok, detector} -> %{keyframe_indicator: detector.(payload) |> bool_to_int()}
        :error -> %{}
      end

    case PayloadFormatResolver.frame_detector(codec) do
      {:ok, detector} ->
        frame_indicator = detector.(payload) |> bool_to_int()
        Map.put(measurements, :frame_indicator, frame_indicator)

      :error ->
        measurements
    end
  end

  defp bool_to_int(true), do: 1
  defp bool_to_int(false), do: 0
end
