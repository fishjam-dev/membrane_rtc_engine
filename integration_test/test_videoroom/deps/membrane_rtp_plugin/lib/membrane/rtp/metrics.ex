defmodule Membrane.RTP.Metrics do
  @moduledoc """
  Defines list of metrics, that can be aggregated based on events from membrane_rtp_plugin.
  """

  alias Telemetry.Metrics

  @doc """
  Returns list of metrics, that can be aggregated based on events from membrane_rtp_plugin.
  """
  @spec metrics() :: [Metrics.t()]
  def metrics() do
    [
      Metrics.counter(
        "inbound-rtp.packets",
        event_name: [Membrane.RTP, :packet, :arrival]
      ),
      Metrics.sum(
        "inbound-rtp.bytes_received",
        event_name: [Membrane.RTP, :packet, :arrival],
        measurement: :bytes
      ),
      Metrics.last_value(
        "inbound-rtp.encoding",
        event_name: [Membrane.RTP, :inbound_track, :new],
        measurement: :encoding
      ),
      Metrics.last_value(
        "inbound-rtp.ssrc",
        event_name: [Membrane.RTP, :inbound_track, :new],
        measurement: :ssrc
      ),
      Metrics.counter(
        "inbound-rtp.markers_received",
        event_name: [Membrane.RTP, :rtp, :marker_received]
      ),
      Metrics.counter(
        "outbound-rtp.markers_sent",
        event_name: [Membrane.RTP, :rtp, :marker_sent]
      ),
      Metrics.counter(
        "rtcp.total_packets_received",
        event_name: [Membrane.RTP, :rtcp, :arrival]
      ),
      Metrics.counter(
        "rtcp.total_packets_sent",
        event_name: [Membrane.RTP, :rtcp, :sent]
      ),
      Metrics.counter(
        "rtcp.nack_sent",
        event_name: [Membrane.RTP, :rtcp, :nack, :sent]
      ),
      Metrics.counter(
        "rtcp.fir_sent",
        event_name: [Membrane.RTP, :rtcp, :fir, :sent]
      ),
      Metrics.counter(
        "rtcp.sender_reports_sent",
        event_name: [Membrane.RTP, :rtcp, :sender_report, :sent]
      ),
      Metrics.counter(
        "rtcp.receiver_reports_sent",
        event_name: [Membrane.RTP, :rtcp, :receiver_report, :sent]
      ),
      Metrics.counter(
        "rtcp.nack_received",
        event_name: [Membrane.RTP, :rtcp, :nack, :arrival]
      ),
      Metrics.counter(
        "rtcp.fir_received",
        event_name: [Membrane.RTP, :rtcp, :fir, :arrival]
      ),
      Metrics.counter(
        "rtcp.pli_received",
        event_name: [Membrane.RTP, :rtcp, :pli, :arrival]
      ),
      Metrics.counter(
        "rtcp.sender_reports_received",
        event_name: [Membrane.RTP, :rtcp, :sender_report, :arrival]
      ),
      Metrics.counter(
        "rtcp.receiver_reports_received",
        event_name: [Membrane.RTP, :rtcp, :receiver_report, :arrival]
      ),
      Metrics.sum(
        "outbound-rtp.rtx_sent",
        event_name: [Membrane.RTP, :rtx, :sent],
        measurement: :amount
      ),
      Metrics.counter(
        "outbound-rtp.packets",
        event_name: [Membrane.RTP, :packet, :sent]
      ),
      Metrics.counter(
        "outbound-rtp.bytes",
        event_name: [Membrane.RTP, :packet, :sent],
        measurement: :bytes
      )
    ]
  end
end
