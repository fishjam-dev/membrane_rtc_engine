defmodule Membrane.RTP.OutboundTrackingSerializer do
  @moduledoc """
  Tracks statistics of outbound packets.

  Besides tracking statistics, tracker can also serialize packet's header and payload stored inside an incoming buffer
  into a proper RTP packet. When encountering header extensions, it remaps its identifiers from locally used extension
  names to integer values expected by the receiver.
  """
  use Membrane.Filter

  require Membrane.Logger
  require Membrane.TelemetryMetrics
  alias Membrane.RTCP.TransportFeedbackPacket.NACK
  alias Membrane.{Buffer, Payload, RemoteStream, RTCP, RTCPEvent, RTP, Time}
  alias Membrane.RTCP.FeedbackPacket.{FIR, PLI}
  alias Membrane.RTCP.TransportFeedbackPacket.NACK
  alias Membrane.RTP.Session.SenderReport

  def_input_pad :input, accepted_format: RTP, demand_mode: :auto

  def_output_pad :output,
    accepted_format: %RemoteStream{type: :packetized, content_format: RTP},
    demand_mode: :auto

  def_input_pad :rtcp_input,
    availability: :on_request,
    accepted_format: _any,
    demand_mode: :auto

  def_output_pad :rtcp_output,
    availability: :on_request,
    accepted_format: %RemoteStream{type: :packetized, content_format: RTCP},
    demand_mode: :auto

  def_options ssrc: [spec: RTP.ssrc_t()],
              payload_type: [spec: RTP.payload_type_t()],
              clock_rate: [spec: RTP.clock_rate_t()],
              extension_mapping: [spec: RTP.SessionBin.rtp_extension_mapping_t()],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: []
              ]

  @packet_sent_telemetry_event [Membrane.RTP, :packet, :sent]
  @marker_sent_telemetry_event [Membrane.RTP, :rtp, :marker_sent]
  @rtcp_sent_telemetry_event [Membrane.RTP, :rtcp, :sent]
  @nack_received_telemetry_event [Membrane.RTP, :rtcp, :nack, :arrival]
  @pli_received_telemetry_event [Membrane.RTP, :rtcp, :pli, :arrival]
  @fir_received_telemetry_event [Membrane.RTP, :rtcp, :fir, :arrival]
  @sender_report_sent_telemetry_event [Membrane.RTP, :rtcp, :sender_report, :sent]

  @telemetry_events [
    @packet_sent_telemetry_event,
    @marker_sent_telemetry_event,
    @rtcp_sent_telemetry_event,
    @nack_received_telemetry_event,
    @pli_received_telemetry_event,
    @fir_received_telemetry_event,
    @sender_report_sent_telemetry_event
  ]

  defmodule State do
    @moduledoc false
    use Bunch.Access

    alias Membrane.RTP

    @type t :: %__MODULE__{
            ssrc: RTP.ssrc_t(),
            payload_type: RTP.payload_type_t(),
            extension_mapping: RTP.SessionBin.rtp_extension_mapping_t(),
            any_buffer_sent?: boolean(),
            rtcp_output_pad: Membrane.Pad.ref_t() | nil,
            stats_acc: %{}
          }

    defstruct ssrc: 0,
              payload_type: 0,
              extension_mapping: %{},
              any_buffer_sent?: false,
              rtcp_output_pad: nil,
              stats_acc: %{
                clock_rate: 0,
                timestamp: 0,
                rtp_timestamp: 0,
                sender_packet_count: 0,
                sender_octet_count: 0
              }
  end

  @impl true
  def handle_init(_ctx, options) do
    for event <- @telemetry_events do
      Membrane.TelemetryMetrics.register(event, options.telemetry_label)
    end

    state =
      %State{}
      |> put_in([:stats_acc, :clock_rate], options.clock_rate)
      |> Map.merge(options |> Map.from_struct() |> Map.drop([:clock_rate]))

    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_input, _id), _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:rtcp_output, _id) = pad,
        %{playback: :playing},
        %{rtcp_output_pad: nil} = state
      ) do
    stream_format = %RemoteStream{type: :packetized, content_format: RTCP}
    {[stream_format: {pad, stream_format}], %{state | rtcp_output_pad: pad}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_output, _id) = pad, _ctx, %{rtcp_output_pad: nil} = state) do
    {[], %{state | rtcp_output_pad: pad}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_output, _id), _ctx, _state) do
    raise "rtcp_output pad can get linked just once"
  end

  @impl true
  def handle_stream_format(:input, _stream_format, _ctx, state) do
    stream_format = %RemoteStream{type: :packetized, content_format: RTP}
    {[stream_format: {:output, stream_format}], state}
  end

  @impl true
  def handle_stream_format(_pad, _stream_format, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:rtcp_input, _id),
        %RTCPEvent{rtcp: %{payload: %PLI{}}},
        _ctx,
        state
      ) do
    Membrane.TelemetryMetrics.execute(
      @pli_received_telemetry_event,
      %{},
      %{},
      state.telemetry_label
    )

    # PLI or FIR reaching OutboundTrackingSerializer means the receiving peer sent it
    # We need to pass it to the sending peer's RTCP.Receiver (in StreamReceiveBin) to get translated again into FIR/PLI with proper SSRCs
    # and then sent to the sender. So the KeyframeRequestEvent, like salmon, starts an upstream journey here trying to reach that peer.
    {[event: {:input, %Membrane.KeyframeRequestEvent{}}], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:rtcp_input, _id),
        %RTCPEvent{rtcp: %{payload: %FIR{}}},
        _ctx,
        state
      ) do
    Membrane.TelemetryMetrics.execute(
      @fir_received_telemetry_event,
      %{},
      %{},
      state.telemetry_label
    )

    {[event: {:input, %Membrane.KeyframeRequestEvent{}}], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:rtcp_input, _id),
        %RTCPEvent{rtcp: %{payload: %NACK{lost_packet_ids: ids}}},
        _ctx,
        state
      ) do
    Membrane.TelemetryMetrics.execute(
      @nack_received_telemetry_event,
      %{},
      %{},
      state.telemetry_label
    )

    # The OutboundRetransmissionController is behind encryptor, so we need to send the event downstream to reach it
    {[event: {:output, %Membrane.RTP.RetransmissionRequestEvent{packet_ids: ids}}], state}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_playing(_ctx, state) do
    if state.rtcp_output_pad do
      stream_format = %RemoteStream{type: :packetized, content_format: RTCP}
      {[stream_format: {state.rtcp_output_pad, stream_format}], state}
    else
      {[], state}
    end
  end

  @impl true
  def handle_process(:input, %Buffer{} = buffer, _ctx, state) do
    state = update_stats(buffer, state)

    %{rtp: rtp_metadata} = buffer.metadata

    supported_extensions = Map.keys(state.extension_mapping)

    extensions =
      rtp_metadata.extensions
      |> Enum.filter(fn extension -> extension.identifier in supported_extensions end)
      |> Enum.map(fn extension ->
        %{extension | identifier: Map.fetch!(state.extension_mapping, extension.identifier)}
      end)

    header =
      struct(RTP.Header, %{
        rtp_metadata
        | ssrc: state.ssrc,
          payload_type: state.payload_type,
          extensions: extensions
      })

    if header.marker do
      Membrane.TelemetryMetrics.execute(
        @marker_sent_telemetry_event,
        %{},
        %{},
        state.telemetry_label
      )
    end

    padding_size = Map.get(rtp_metadata, :padding_size, 0)

    payload =
      RTP.Packet.serialize(%RTP.Packet{header: header, payload: buffer.payload},
        padding_size: padding_size
      )

    buffer = %Buffer{buffer | payload: payload}

    Membrane.TelemetryMetrics.execute(
      @packet_sent_telemetry_event,
      %{bytes: byte_size(buffer.payload)},
      %{},
      state.telemetry_label
    )

    {[buffer: {:output, buffer}], %{state | any_buffer_sent?: true}}
  end

  @impl true
  def handle_parent_notification(:send_stats, ctx, state) do
    %{rtcp_output_pad: rtcp_output} = state

    if rtcp_output && not ctx.pads[rtcp_output].end_of_stream? do
      stats = get_stats(state)

      actions =
        %{state.ssrc => stats}
        |> SenderReport.generate_report()
        |> Enum.map(&Membrane.RTCP.Packet.serialize(&1))
        |> Enum.map(&{:buffer, {rtcp_output, %Membrane.Buffer{payload: &1}}})

      for _buffer_action <- actions do
        Membrane.TelemetryMetrics.execute(
          @rtcp_sent_telemetry_event,
          %{},
          %{},
          state.telemetry_label
        )

        Membrane.TelemetryMetrics.execute(
          @sender_report_sent_telemetry_event,
          %{},
          %{},
          state.telemetry_label
        )
      end

      {actions, %{state | any_buffer_sent?: false}}
    else
      {[], state}
    end
  end

  defp get_stats(%State{any_buffer_sent?: false}), do: :no_stats
  defp get_stats(%State{stats_acc: stats}), do: stats

  defp update_stats(%Buffer{payload: payload, metadata: metadata}, state) do
    %{
      sender_octet_count: octet_count,
      sender_packet_count: packet_count
    } = state.stats_acc

    updated_stats = %{
      clock_rate: state.stats_acc.clock_rate,
      sender_octet_count: octet_count + Payload.size(payload),
      sender_packet_count: packet_count + 1,
      timestamp: Time.vm_time(),
      rtp_timestamp: metadata.rtp.timestamp
    }

    Map.put(state, :stats_acc, updated_stats)
  end
end
