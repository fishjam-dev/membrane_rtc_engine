defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandler do
  @moduledoc false
  use Membrane.Endpoint

  require Logger

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTC.Engine.Endpoint.ExWebRTC, as: EndpointExWebRTC

  alias ExWebRTC.{
    ICECandidate,
    MediaStreamTrack,
    PeerConnection,
    RTPCodecParameters,
    RTPReceiver,
    RTPTransceiver,
    SessionDescription
  }

  # alias ExWebRTC.Media.Ogg
  # alias ExWebRTC.RTP.Depayloader

  def_options endpoint_id: [
                spec: String.t(),
                description: "Id of the parent endpoint"
              ],
              ice_port_range: [
                spec: Enumerable.t(non_neg_integer()),
                description: "Range of ports that ICE will use for gathering host candidates."
              ]

  def_input_pad :input,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :output,
    accepted_format: _any,
    availability: :on_request,
    flow_control: :push

  @ice_servers [
    %{urls: "stun:stun.l.google.com:19302"}
  ]

  # @video_codecs [
  #   %RTPCodecParameters{
  #     payload_type: 96,
  #     mime_type: "video/VP8",
  #     clock_rate: 90_000,
  #     rtcp_fbs: [%ExSDP.Attribute.RTCPFeedback{pt: 96, feedback_type: :nack}]
  #   },
  #   %RTPCodecParameters{
  #     payload_type: 97,
  #     mime_type: "video/rtx",
  #     clock_rate: 90_000,
  #     sdp_fmtp_line: %ExSDP.Attribute.FMTP{pt: 97, apt: 96}
  #   }
  # ]

  # @audio_file "/Users/roznawsk/fj/membrane_rtc_engine/audio.ogg"

  # @audio_codecs [
  #   %RTPCodecParameters{
  #     payload_type: 111,
  #     mime_type: "audio/opus",
  #     clock_rate: 48_000,
  #     channels: 2
  #   }
  # ]

  @opts [
    ice_servers: @ice_servers
    # audio_codecs: @audio_codecs,
    # video_codecs: @video_codecs
  ]

  @impl true
  def handle_init(_ctx, opts) do
    %{endpoint_id: endpoint_id} = opts

    pc_options =
      %{
        ice_port_range: opts.ice_port_range
      }
      |> Enum.filter(fn {_k, v} -> not is_nil(v) end)
      |> Keyword.merge(@opts)

    dbg(pc_options)

    {:ok, pc} = PeerConnection.start_link(pc_options)

    # {:ok, audio_writer} = Ogg.Writer.open(@audio_file)
    # {:ok, audio_depayloader} = @audio_codecs |> hd() |> Depayloader.new()

    state = %{
      pc: pc,
      endpoint_id: endpoint_id,
      # maps engine track_id to rtc track_id
      outbound_tracks: %{},
      # maps rtc track_id to engine track_id
      inbound_tracks: %{},
      queued_tracks: %{},
      during_negotiation?: false
      # audio_depayloader: audio_depayloader,
      # audio_writer: audio_writer,
    }

    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {_track_id, _rid}) = pad, _ctx, state) do
    {[stream_format: {pad, %Membrane.RTP{}}], state}
  end

  @impl true
  def handle_pad_added(_pad, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_buffer(Pad.ref(:input, engine_track_id), buffer, _ctx, state)
      when is_map_key(state.outbound_tracks, engine_track_id) do
    %Buffer{
      pts: timestamp,
      payload: payload,
      metadata: %{rtp: rtp}
    } = buffer

    track_id = Map.fetch!(state.outbound_tracks, engine_track_id)

    packet =
      ExRTP.Packet.new(
        payload,
        payload_type: rtp.payload_type,
        sequence_number: rtp.sequence_number,
        timestamp: timestamp,
        ssrc: rtp.ssrc,
        csrc: rtp.csrc,
        marker: rtp.marker,
        padding: rtp.padding_size
      )

    extensions = if is_list(rtp.extensions), do: rtp.extensions, else: []

    packet =
      Enum.reduce(extensions, packet, fn extension, packet ->
        ExRTP.Packet.add_extension(packet, extension)
      end)

    if Enum.random(0..1000) == 0 do
      dbg({track_id, packet})
    end

    :ok = PeerConnection.send_rtp(state.pc, track_id, packet)

    {[], state}
  end

  @impl true
  def handle_buffer(Pad.ref(:input, track_id), _buffer, _ctx, state) do
    Logger.warning("Received buffer from unknown track #{track_id}")
    {[], state}
  end

  @impl true
  def handle_parent_notification({:offer, event, outbound_tracks}, _ctx, state) do
    %{"sdpOffer" => offer, "midToTrackId" => mid_to_track_id} = event

    new_outbound_tracks =
      Map.filter(outbound_tracks, fn {track_id, _track} ->
        not Map.has_key?(state.outbound_tracks, track_id)
      end)

    PeerConnection.get_transceivers(state.pc)
    |> Enum.map(fn t ->
      Map.take(t, [:id, :kind, :direction, :current_direction])
      |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    end)
    |> IO.inspect(label: :before)

    offer = SessionDescription.from_json(offer)
    :ok = PeerConnection.set_remote_description(state.pc, offer)

    PeerConnection.get_transceivers(state.pc)
    |> Enum.map(fn t ->
      Map.take(t, [:id, :kind, :direction, :current_direction])
      |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    end)
    |> IO.inspect(label: :in_between)

    outbound_transceivers =
      state.pc
      |> PeerConnection.get_transceivers()
      |> Enum.filter(fn transceiver ->
        not Map.has_key?(mid_to_track_id, transceiver.mid)
      end)

    {track_ids, _out_trans} =
      new_outbound_tracks
      |> Enum.map_reduce(outbound_transceivers, fn {engine_track_id, engine_track}, outbound_transceivers ->
        track =
          MediaStreamTrack.new(engine_track.type, [MediaStreamTrack.generate_stream_id()])

        transceiver = Enum.find(outbound_transceivers, fn transceiver ->
          transceiver.kind == track.kind
        end)

        outbound_transceivers = List.delete(outbound_transceivers, transceiver)

        # {:ok, sender} = PeerConnection.add_track(state.pc, track)
        # {:ok, transceiver} = PeerConnection.add_transceiver(state.pc, track, direction: :sendonly)

        PeerConnection.set_transceiver_direction(state.pc, transceiver.id, :sendonly)
        PeerConnection.replace_track(state.pc, transceiver.sender.id, track)

        {{engine_track_id, track.id, transceiver.sender.id}, outbound_transceivers}
      end)

    transceivers = PeerConnection.get_transceivers(state.pc)

    mid_to_track_id =
      track_ids
      |> Enum.map(fn {track_id, id, sender_id} ->
        mid =
          Enum.find(transceivers, fn transceiver ->
            transceiver.sender.id == sender_id
          end)
          |> then(& &1.mid)

        dbg({track_id, to_string(mid), id})

        {to_string(mid), track_id}
      end)
      |> Map.new()

    outbound_tracks =
      Map.new(track_ids, fn {engine_id, id, _sender_id} ->
        {engine_id, id}
      end)
      |> Map.merge(state.outbound_tracks)

    # dbg(outbound_tracks)
    # dbg(trans)

    {:ok, answer} = PeerConnection.create_answer(state.pc)
    :ok = PeerConnection.set_local_description(state.pc, answer)

    {tracks, state} =
      receive_new_tracks()
      |> make_tracks(state)

    PeerConnection.get_transceivers(state.pc)
    # |> Enum.map(fn t ->
    #   Map.take(t, [:id, :kind, :direction, :current_direction])
    #   |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    # end)
    |> IO.inspect(label: :after)

    actions =
      [notify_parent: {:answer, SessionDescription.to_json(answer), mid_to_track_id}] ++
        if Enum.empty?(tracks), do: [], else: [notify_parent: {:tracks, tracks}]

    {actions, %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_parent_notification({:candidate, candidate}, _ctx, state) do
    # dbg(candidate)
    dbg(candidate)
    candidate = ICECandidate.from_json(candidate)
    :ok = PeerConnection.add_ice_candidate(state.pc, candidate)

    {[], state}
  end

  @impl true
  def handle_parent_notification(_msg, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:output, {engine_track_id, variant}),
        %Membrane.KeyframeRequestEvent{},
        _ctx,
        state
      ) do
    {rtc_track_id, _id} =
      Enum.find(state.inbound_tracks, fn {_rtc_track_id, track_id} ->
        track_id == engine_track_id
      end)

    _rid = EndpointExWebRTC.to_rid(variant)
    PeerConnection.send_pli(state.pc, rtc_track_id, nil)

    {[], state}
  end

  @impl true
  def handle_info({:ex_webrtc, _from, msg}, ctx, state) do
    handle_webrtc_msg(msg, ctx, state)
  end

  defp handle_webrtc_msg({:ice_candidate, candidate}, _ctx, state) do
    msg = {:candidate, ICECandidate.to_json(candidate)}
    {[notify_parent: msg], state}
  end

  defp handle_webrtc_msg({:track, _track}, _ctx, state) do
    raise("We do not expect to receive any tracks")
    {[], state}
  end

  defp handle_webrtc_msg({:rtp, track_id, rid, packet}, ctx, state) do
    # TEMPORARY
    rid = if rid == nil, do: :high, else: rid

    # {opus_packet, depayloader} = Depayloader.depayload(state.audio_depayloader, packet)
    # {:ok, audio_writer} = Ogg.Writer.write_packet(state.audio_writer, opus_packet)
    # audio_writer = state.audio_writer

    actions =
      with {:ok, engine_track_id} <- Map.fetch(state.inbound_tracks, track_id),
           pad <- Pad.ref(:output, {engine_track_id, rid}),
           true <- Map.has_key?(ctx.pads, pad) do
        rtp =
          packet
          |> Map.from_struct()
          |> Map.take([
            :csrc,
            :extensions,
            :marker,
            :padding_size,
            :payload_type,
            :sequence_number,
            :ssrc,
            :timestamp
          ])

        buffer = %Buffer{
          pts: packet.timestamp,
          payload: packet.payload,
          metadata: %{rtp: rtp}
        }

        [buffer: {pad, buffer}]
      else
        _other -> []
      end

    {actions, state}
  end

  defp handle_webrtc_msg({:signaling_state_change, :stable}, _ctx, state) do
    {[notify_parent: :negotiation_done], state}
  end

  defp handle_webrtc_msg({:rtcp, packets}, _ctx, state) do
    actions =
      Enum.flat_map(packets, fn
        {_track_id, %ExRTCP.Packet.PayloadFeedback.PLI{}} -> []
        {_track_id, _other} -> []
      end)

    {actions, state}
  end

  defp handle_webrtc_msg(_msg, _ctx, state) do
    {[], state}
  end

  defp receive_new_tracks(), do: do_receive_new_tracks([])

  defp do_receive_new_tracks(acc) do
    receive do
      {:ex_webrtc, pc, {:track, track}} ->
        transceiver =
          PeerConnection.get_transceivers(pc)
          |> Enum.find(fn transceiver ->
            transceiver.receiver.track.id == track.id
          end)

        PeerConnection.set_transceiver_direction(pc, transceiver.id, :sendrecv) |> dbg()

        PeerConnection.replace_track(pc, transceiver.sender.id, MediaStreamTrack.new(track.kind))
        |> dbg()

        PeerConnection.set_transceiver_direction(pc, transceiver.id, :recvonly) |> dbg()

        do_receive_new_tracks([track | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  defp make_tracks(tracks, state) do
    transceivers = PeerConnection.get_transceivers(state.pc)
    do_make_tracks(tracks, transceivers, state, [])
  end

  defp do_make_tracks([], _transceivers, state, acc), do: {Enum.reverse(acc), state}

  defp do_make_tracks([track | tracks], transceivers, state, acc) do
    codec =
      Enum.find_value(transceivers, fn
        %RTPTransceiver{receiver: %RTPReceiver{track: ^track, codec: codec}} -> codec
        _other -> nil
      end)

    %MediaStreamTrack{id: id, kind: kind} = track

    encoding =
      case codec.mime_type do
        "audio/opus" -> :OPUS
        "video/VP8" -> :VP8
        "video/H264" -> :H264
      end

    # TODO stream id
    engine_track =
      Track.new(
        kind,
        Track.stream_id(),
        state.endpoint_id,
        encoding,
        codec.clock_rate,
        codec.sdp_fmtp_line
      )

    inbound_tracks = Map.put(state.inbound_tracks, id, engine_track.id)
    state = %{state | inbound_tracks: inbound_tracks}
    do_make_tracks(tracks, transceivers, state, [engine_track | acc])
  end
end
