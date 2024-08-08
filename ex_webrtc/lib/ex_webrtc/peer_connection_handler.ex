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

  def_options endpoint_id: [
                spec: String.t(),
                description: "Id of the parent endpoint"
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

  @video_codecs [
    %RTPCodecParameters{
      payload_type: 96,
      mime_type: "video/VP8",
      clock_rate: 90_000,
      rtcp_fbs: [%ExSDP.Attribute.RTCPFeedback{pt: 96, feedback_type: :nack}]
    },
    %RTPCodecParameters{
      payload_type: 97,
      mime_type: "video/rtx",
      clock_rate: 90_000,
      sdp_fmtp_line: %ExSDP.Attribute.FMTP{pt: 97, apt: 96}
    }
  ]

  @audio_codecs [
    %RTPCodecParameters{
      payload_type: 111,
      mime_type: "audio/opus",
      clock_rate: 48_000,
      channels: 2
    }
  ]

  @opts [
    ice_servers: @ice_servers,
    audio_codecs: @audio_codecs,
    video_codecs: @video_codecs
  ]

  @impl true
  def handle_init(_ctx, opts) do
    %{endpoint_id: endpoint_id} = opts

    {:ok, pc} = PeerConnection.start_link(@opts)

    state = %{
      pc: pc,
      endpoint_id: endpoint_id,
      # maps engine track_id to rtc track_id
      outbound_tracks: %{},
      # maps rtc track_id to engine track_id
      inbound_tracks: %{},
      queued_tracks: %{},
      during_negotiation?: false
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

    packet =
      Enum.reduce(rtp.extensions, packet, fn extension, packet ->
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
  def handle_parent_notification(:add_peer_tracks, _ctx, state) do
    stream_id = MediaStreamTrack.generate_stream_id()
    audio = MediaStreamTrack.new(:audio, [stream_id])
    video = MediaStreamTrack.new(:video, [stream_id])

    {:ok, _t_audio} = PeerConnection.add_transceiver(state.pc, audio, direction: :recvonly)
    {:ok, _t_video} = PeerConnection.add_transceiver(state.pc, video, direction: :recvonly)

    {:ok, offer} = PeerConnection.create_offer(state.pc)
    :ok = PeerConnection.set_local_description(state.pc, offer)

    {[notify_parent: {:offer, SessionDescription.to_json(offer)}],
     %{state | during_negotiation?: true}}
  end

  @impl true
  def handle_parent_notification({:answer, answer}, _ctx, state) do
    answer = SessionDescription.from_json(answer)
    :ok = PeerConnection.set_remote_description(state.pc, answer)

    {actions, state} = maybe_add_queued_tracks(state)

    {actions, %{state | during_negotiation?: false}}
  end

  @impl true
  def handle_parent_notification({:candidate, candidate}, _ctx, state) do
    # dbg(candidate)
    candidate = Jason.decode!(candidate)
    dbg(candidate)
    candidate = ICECandidate.from_json(candidate)
    :ok = PeerConnection.add_ice_candidate(state.pc, candidate)

    {[], state}
  end

  @impl true
  def handle_parent_notification(
        {:new_tracks, engine_tracks},
        _ctx,
        %{during_negotiation?: true} = state
      ) do
    dbg(engine_tracks)
    queued_tracks = Map.merge(state.queued_tracks, engine_tracks)
    dbg(queued_tracks)
    {[], %{state | queued_tracks: queued_tracks}}
  end

  @impl true
  def handle_parent_notification({:new_tracks, engine_tracks}, _ctx, state) do
    state = %{state | queued_tracks: Map.merge(state.queued_tracks, engine_tracks)}
    maybe_add_queued_tracks(state)
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

  defp handle_webrtc_msg({:track, track}, _ctx, state) do
    dbg("Received track")

    transceivers = PeerConnection.get_transceivers(state.pc)

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

    actions = [notify_parent: {:track, engine_track}]
    {actions, %{state | inbound_tracks: inbound_tracks}}
  end

  defp handle_webrtc_msg({:rtp, track_id, rid, packet}, ctx, state) do
    # TEMPORARY
    rid = if rid == nil, do: :high, else: rid

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

  defp maybe_add_queued_tracks(%{queued_tracks: tracks} = state) when map_size(tracks) == 0 do
    {[], state}
  end

  defp maybe_add_queued_tracks(state) do
    dbg(state.queued_tracks)

    PeerConnection.get_transceivers(state.pc)
    |> Enum.map(fn t ->
      Map.take(t, [:id, :kind, :direction, :current_direction])
      |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    end)
    |> IO.inspect(label: :before)

    new_outbound_tracks =
      state.queued_tracks
      |> Map.new(fn {engine_track_id, engine_track} ->
        track = MediaStreamTrack.new(engine_track.type, [engine_track.stream_id])

        {:ok, _sender} = PeerConnection.add_track(state.pc, track)

        {engine_track_id, track.id}
      end)

    PeerConnection.get_transceivers(state.pc)
    |> Enum.map(fn t ->
      Map.take(t, [:id, :kind, :direction, :current_direction])
      |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    end)
    |> IO.inspect(label: :in_between)

    {:ok, offer} = PeerConnection.create_offer(state.pc)
    :ok = PeerConnection.set_local_description(state.pc, offer)

    PeerConnection.get_transceivers(state.pc)
    |> Enum.map(fn t ->
      Map.take(t, [:id, :kind, :direction, :current_direction])
      |> Map.merge(%{sender_track: t.sender.track, receiver_track: t.receiver.track})
    end)
    |> IO.inspect(label: :after)

    outbound_tracks =
      Map.merge(state.outbound_tracks, new_outbound_tracks)

    dbg(outbound_tracks)

    {[notify_parent: {:offer, SessionDescription.to_json(offer)}],
     %{state | outbound_tracks: outbound_tracks, queued_tracks: %{}, during_negotiation?: true}}
  end
end
