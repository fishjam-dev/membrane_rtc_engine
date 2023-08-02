defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Metrics do
  @moduledoc """
  Defines list of metrics, that Reporter instance can aggregate by listening on events emitted in RTC Engine.
  Suggested Reporter implementation is `Membrane.TelemetryMetrics.Reporter` from
  [`membrane_telemetry_metrics`](https://github.com/membraneframework/membrane_telemetry_metrics).
  You can see usage example in [`membrane_videoroom`](https://github.com/membraneframework/membrane_videoroom).

  See also `Metrics` guide for more details and example report that you can obtain with
  `Membrane.TelemetryMetrics.Reporter`.
  """

  require Membrane.TelemetryMetrics

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTP.PayloadFormatResolver

  @rtp_packet_arrival_event [Membrane.RTC.Engine.Endpoint.WebRTC, :RTP, :packet, :arrival]
  @variant_switched_event [Membrane.RTC.Engine.Endpoint.WebRTC, :RTP, :variant, :switched]
  @paddings_sent_event [Membrane.RTC.Engine.Endpoint.WebRTC, :RTP, :paddings, :sent]
  @bandwidth_event [Membrane.RTC.Engine.Endpoint.WebRTC, :endpoint, :bandwidth]

  @spec metrics() :: [Telemetry.Metrics.t()]
  def metrics() do
    Enum.concat([
      endpoint_metrics(),
      Membrane.RTP.Metrics.metrics(),
      Membrane.ICE.Metrics.metrics(),
      Membrane.WebRTC.Metrics.metrics()
    ])
  end

  defp endpoint_metrics() do
    [
      Telemetry.Metrics.sum(
        "inbound-rtp.frames",
        event_name: @rtp_packet_arrival_event,
        measurement: :frame_indicator
      ),
      Telemetry.Metrics.sum(
        "inbound-rtp.keyframes",
        event_name: @rtp_packet_arrival_event,
        measurement: :keyframe_indicator
      ),
      Telemetry.Metrics.last_value(
        "outbound-rtp.variant",
        event_name: @variant_switched_event,
        measurement: :variant
      ),
      Telemetry.Metrics.last_value(
        "outbound-rtp.variant-reason",
        event_name: @variant_switched_event,
        measurement: :reason
      ),
      Telemetry.Metrics.sum(
        "outbound-rtp.paddings_sent",
        event_name: @paddings_sent_event,
        measurement: :num
      ),
      Telemetry.Metrics.sum(
        "outbound-rtp.paddings_bytes_sent",
        event_name: @paddings_sent_event,
        measurement: :bytes
      ),
      Telemetry.Metrics.last_value(
        "endpoint.bandwidth",
        event_name: @bandwidth_event,
        measurement: :bandwidth
      ),
      Telemetry.Metrics.last_value(
        "endpoint.metadata",
        event_name: [Membrane.RTC.Engine.Endpoint.WebRTC, :endpoint, :metadata, :event],
        measurement: :metadata
      ),
      Telemetry.Metrics.last_value(
        "track.metadata",
        event_name: [Membrane.RTC.Engine.Endpoint.WebRTC, :track, :metadata, :event],
        measurement: :metadata
      )
    ]
  end

  @doc false
  @spec telemetry_register(Membrane.TelemetryMetrics.label()) :: :ok
  def telemetry_register(telemetry_label) do
    Membrane.TelemetryMetrics.register(@rtp_packet_arrival_event, telemetry_label)
    :ok
  end

  @doc false
  @spec register_bandwidth_event(Membrane.TelemetryMetrics.label()) :: :ok
  def register_bandwidth_event(telemetry_label) do
    Membrane.TelemetryMetrics.register(@bandwidth_event, telemetry_label)
    :ok
  end

  @doc false
  @spec register_variant_switched_event(Membrane.TelemetryMetrics.label()) :: :ok
  def register_variant_switched_event(telemetry_label) do
    Membrane.TelemetryMetrics.register(@variant_switched_event, telemetry_label)
    :ok
  end

  @doc false
  @spec register_paddings_sent_event(Membrane.TelemetryMetrics.label()) :: :ok
  def register_paddings_sent_event(telemetry_label) do
    Membrane.TelemetryMetrics.register(@paddings_sent_event, telemetry_label)
    :ok
  end

  @doc false
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

  @doc false
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

  @doc false
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

  @doc false
  @spec emit_paddings_sent_event(
          non_neg_integer(),
          non_neg_integer(),
          Membrane.TelemetryMetrics.label()
        ) :: :ok
  def emit_paddings_sent_event(num, bytes, telemetry_label) do
    Membrane.TelemetryMetrics.execute(
      @paddings_sent_event,
      %{num: num, bytes: bytes},
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
