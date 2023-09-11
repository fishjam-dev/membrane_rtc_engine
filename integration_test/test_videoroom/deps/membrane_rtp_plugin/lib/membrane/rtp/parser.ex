defmodule Membrane.RTP.Parser do
  @moduledoc """
  Identifies RTP/RTCP packets, then tries to parse RTP packet (parsing header and preparing payload)
  and forwards RTCP packet to `:rtcp_output` pad unchanged.

  ## Encrypted packets
  In case of SRTP/SRTCP the parser tries to parse just the header of the RTP packet as the packet's payload
  is encrypted and must be passed as a whole to the decryptor. The whole packet remains unchanged but
  the parsed header gets  attached to `Membrane.Buffer`'s metadata.

  SRTP is treated the same as RTCP and all packets gets forwarded to `:rtcp_output` pad.

  ## Parsed packets
  In both cases, encrypted and unencryptd, parsed header is put into the metadata field in `Membrane.Buffer` under `:rtp` key.
  with the following metadata `:timestamp`, `:sequence_number`, `:ssrc`, `:payload_type`,
  `:marker`, `:extension`. See `Membrane.RTP.Header` for their meaning and specifications.
  """

  use Membrane.Filter

  require Membrane.Logger
  alias Membrane.Buffer
  alias Membrane.{RemoteStream, RTCP, RTCPEvent, RTP}

  @metadata_fields [
    :timestamp,
    :sequence_number,
    :ssrc,
    :csrcs,
    :payload_type,
    :marker,
    :extensions
  ]

  def_options secure?: [
                type: :boolean,
                default: false,
                description: """
                Specifies whether Parser should expect packets that are encrypted or not.
                Requires adding [srtp](https://github.com/membraneframework/elixir_libsrtp) dependency to work.
                """
              ]

  def_input_pad :input,
    accepted_format:
      %RemoteStream{type: :packetized, content_format: cf} when cf in [nil, RTP, RTCP],
    demand_mode: :auto

  def_output_pad :output, accepted_format: RTP, demand_mode: :auto

  def_output_pad :rtcp_output,
    mode: :push,
    accepted_format: %RemoteStream{content_format: RTCP, type: :packetized},
    availability: :on_request

  @impl true
  def handle_init(_ctx, opts) do
    {[], %{rtcp_output_pad: nil, secure?: opts.secure?}}
  end

  @impl true
  def handle_stream_format(:input, _stream_format, _ctx, state) do
    {[stream_format: {:output, %RTP{}}], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_output, _ref) = pad, %{playback_state: :playing}, state) do
    actions = [stream_format: {pad, %RemoteStream{content_format: RTCP, type: :packetized}}]
    {actions, %{state | rtcp_output_pad: pad}}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_output, _ref) = pad, _ctx, state) do
    {[], %{state | rtcp_output_pad: pad}}
  end

  @impl true
  def handle_playing(_ctx, %{rtcp_output_pad: pad} = state) when pad != nil do
    actions = [stream_format: {pad, %RemoteStream{content_format: RTCP, type: :packetized}}]
    {actions, state}
  end

  @impl true
  def handle_playing(ctx, state) do
    super(ctx, state)
  end

  @impl true
  def handle_process(:input, %Buffer{payload: payload, metadata: metadata} = buffer, _ctx, state) do
    with :rtp <- RTP.Packet.identify(payload),
         {:ok,
          %{packet: packet, padding_size: padding_size, total_header_size: total_header_size}} <-
           RTP.Packet.parse(payload, state.secure?) do
      %RTP.Packet{payload: payload, header: header} = packet

      rtp =
        header
        |> Map.take(@metadata_fields)
        |> Map.merge(%{padding_size: padding_size, total_header_size: total_header_size})

      metadata = Map.put(metadata, :rtp, rtp)
      {[buffer: {:output, %Buffer{payload: payload, metadata: metadata}}], state}
    else
      :rtcp ->
        case state.rtcp_output_pad do
          nil -> {[], state}
          pad -> {[buffer: {pad, buffer}], state}
        end

      {:error, reason} ->
        Membrane.Logger.debug("""
        Couldn't parse rtp packet:
        #{inspect(payload, limit: :infinity)}
        Reason: #{inspect(reason)}. Ignoring packet.
        """)

        {[], state}
    end
  end

  @impl true
  def handle_event(:output, %RTCPEvent{} = event, _ctx, state) do
    case state.rtcp_output_pad do
      nil ->
        {[], state}

      pad ->
        {[event: {pad, event}], state}
    end
  end

  @impl true
  def handle_event(pad, event, ctx, state), do: super(pad, event, ctx, state)
end
