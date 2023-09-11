defmodule Membrane.RTCP.Receiver do
  @moduledoc """
  Element exchanging RTCP packets and RTCP receiver statistics.
  """
  use Membrane.Filter

  require Membrane.Logger
  require Membrane.TelemetryMetrics

  alias Membrane.RTCP.{
    FeedbackPacket,
    ReceiverReport,
    SenderReportPacket,
    TransportFeedbackPacket
  }

  alias Membrane.RTCPEvent
  alias Membrane.Time
  alias Membrane.{RTCP, RTP}

  def_input_pad :input, accepted_format: _any, demand_mode: :auto
  def_output_pad :output, accepted_format: _any, demand_mode: :auto

  def_options local_ssrc: [spec: RTP.ssrc_t()],
              remote_ssrc: [spec: RTP.ssrc_t()],
              report_interval: [spec: Membrane.Time.t() | nil, default: nil],
              telemetry_label: [spec: Membrane.TelemetryMetrics.label(), default: []]

  @fir_throttle_duration Application.compile_env(
                           :membrane_rtp_plugin,
                           :fir_throttle_duration_ms,
                           500
                         )
                         |> Membrane.Time.milliseconds()

  @fir_sent_telemetry_event [Membrane.RTP, :rtcp, :fir, :sent]
  @nack_sent_telemetry_event [Membrane.RTP, :rtcp, :nack, :sent]
  @sender_report_received_telemetry_event [Membrane.RTP, :rtcp, :sender_report, :arrival]
  @receiver_report_sent_telemetry_event [Membrane.RTP, :rtcp, :receiver_report, :sent]

  @impl true
  def handle_init(_ctx, opts) do
    [
      @fir_sent_telemetry_event,
      @nack_sent_telemetry_event,
      @sender_report_received_telemetry_event,
      @receiver_report_sent_telemetry_event
    ]
    |> Enum.each(fn event ->
      Membrane.TelemetryMetrics.register(event, opts.telemetry_label)
    end)

    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{fir_seq_num: 0, last_fir_timestamp: 0, sr_info: %{}})

    {[], state}
  end

  @impl true
  def handle_playing(_ctx, state) do
    report_timer =
      if state.report_interval,
        do: [start_timer: {:report_timer, state.report_interval}],
        else: []

    {report_timer, state}
  end

  @impl true
  def handle_tick(:report_timer, _ctx, state) do
    {[event: {:output, %ReceiverReport.StatsRequestEvent{}}], state}
  end

  @impl true
  def handle_event(:input, %RTCPEvent{rtcp: %SenderReportPacket{} = rtcp} = event, _ctx, state) do
    emit_telemetry_event(@sender_report_received_telemetry_event, state)

    <<_wallclock_ts_upper_16_bits::16, wallclock_ts_middle_32_bits::32,
      _wallclock_ts_lower_16_bits::16>> =
      Time.to_ntp_timestamp(rtcp.sender_info.wallclock_timestamp)

    sr_info = %{
      cut_wallclock_ts: wallclock_ts_middle_32_bits,
      arrival_ts: event.arrival_timestamp
    }

    {[], %{state | sr_info: sr_info}}
  end

  @impl true
  def handle_event(:input, %RTCPEvent{} = event, _ctx, state) do
    Membrane.Logger.error("Unexpected RTCPEvent: #{inspect(event)}")
    {[], state}
  end

  @impl true
  def handle_event(:output, %ReceiverReport.StatsEvent{stats: :no_stats}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_event(:output, %ReceiverReport.StatsEvent{stats: stats}, _ctx, state) do
    now = Time.vm_time()
    delay_since_sr = now - Map.get(state.sr_info, :arrival_ts, now)

    report_block = %RTCP.ReportPacketBlock{
      ssrc: state.remote_ssrc,
      fraction_lost: stats.fraction_lost,
      total_lost: stats.total_lost,
      highest_seq_num: stats.highest_seq_num,
      interarrival_jitter: trunc(stats.interarrival_jitter),
      last_sr_timestamp: Map.get(state.sr_info, :cut_wallclock_ts, 0),
      # delay_since_sr is expressed in 1/65536 seconds, see https://tools.ietf.org/html/rfc3550#section-6.4.1
      delay_since_sr: Time.round_to_seconds(65_536 * delay_since_sr)
    }

    packet = %RTCP.ReceiverReportPacket{ssrc: state.local_ssrc, reports: [report_block]}

    emit_telemetry_event(@receiver_report_sent_telemetry_event, state)

    {[event: {:input, %RTCPEvent{rtcp: packet}}], state}
  end

  @impl true
  def handle_event(:output, %Membrane.KeyframeRequestEvent{}, _ctx, state) do
    send_fir(state)
  end

  @impl true
  def handle_event(:output, %RTP.RetransmissionRequestEvent{packet_ids: ids}, _ctx, state) do
    rtcp = %TransportFeedbackPacket{
      media_ssrc: state.remote_ssrc,
      sender_ssrc: state.local_ssrc,
      payload: %TransportFeedbackPacket.NACK{
        lost_packet_ids: ids
      }
    }

    emit_telemetry_event(@nack_sent_telemetry_event, state)

    event = %RTCPEvent{rtcp: rtcp}
    Membrane.Logger.debug("Sending NACK to #{state.remote_ssrc} with ids #{inspect(ids)}")
    {[event: {:input, event}], state}
  end

  @impl true
  def handle_event(pad, event, ctx, state), do: super(pad, event, ctx, state)

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    {[buffer: {:output, buffer}], state}
  end

  defp send_fir(state) do
    now = Time.vm_time()

    if now - state.last_fir_timestamp > @fir_throttle_duration do
      rtcp = %FeedbackPacket{
        origin_ssrc: state.local_ssrc,
        payload: %FeedbackPacket.FIR{
          target_ssrc: state.remote_ssrc,
          seq_num: state.fir_seq_num
        }
      }

      emit_telemetry_event(@fir_sent_telemetry_event, state)

      event = %RTCPEvent{rtcp: rtcp}
      state = %{state | fir_seq_num: state.fir_seq_num + 1, last_fir_timestamp: now}
      Membrane.Logger.info("Sending FIR to #{state.remote_ssrc}")
      {[event: {:input, event}], state}
    else
      Membrane.Logger.debug("Not sending FIR to #{state.remote_ssrc} due to throttling")
      {[], state}
    end
  end

  defp emit_telemetry_event(event, state),
    do: Membrane.TelemetryMetrics.execute(event, %{}, %{}, state.telemetry_label)
end
