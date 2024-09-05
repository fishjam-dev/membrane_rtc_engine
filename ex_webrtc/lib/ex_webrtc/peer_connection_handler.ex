defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandler do
  @moduledoc false
  use Membrane.Endpoint

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Track

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
      outbound_tracks: %{},
      inbound_tracks: %{}
    }

    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {_track_id, _rid}) = pad, _ctx, state) do
    {[stream_format: {pad, %Membrane.RTP{}}], state}
  end

  @impl true
  def handle_buffer(_pad, _buffer, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:offer, offer, outbound_tracks}, _ctx, state) do
    IO.puts(Map.get(offer, "sdp"))

    offer = SessionDescription.from_json(offer)
    :ok = PeerConnection.set_remote_description(state.pc, offer)

    trans =
      PeerConnection.get_transceivers(state.pc)
      |> Enum.map(&Map.take(&1, [:id, :kind, :mid, :direction, :current_direction]))

    dbg(trans)

    track_id_to_sender_id =
      outbound_tracks
      |> Enum.map(fn {track_id, engine_track} ->
        track =
          MediaStreamTrack.new(engine_track.type, [MediaStreamTrack.generate_stream_id()])
          |> dbg()

        {:ok, sender} = PeerConnection.add_track(state.pc, track)
        {track_id, sender.id}
      end)

    transceivers = PeerConnection.get_transceivers(state.pc)

    mid_to_track_id =
      track_id_to_sender_id
      |> Enum.map(fn {track_id, sender_id} ->
        mid =
          Enum.find(transceivers, fn transceiver ->
            transceiver.sender.id == sender_id
          end)
          |> then(& &1.mid)

        {to_string(mid), track_id}
      end)
      |> Map.new()

    dbg(mid_to_track_id)

    trans =
      PeerConnection.get_transceivers(state.pc)

    # |> Enum.map(&Map.take(&1, [:id, :kind, :mid, :direction, :current_direction]))

    dbg(trans)

    {:ok, answer} = PeerConnection.create_answer(state.pc)
    :ok = PeerConnection.set_local_description(state.pc, answer)

    {tracks, state} =
      receive_new_tracks()
      |> make_tracks(state)

    actions =
      [notify_parent: {:answer, SessionDescription.to_json(answer), mid_to_track_id}] ++
        if Enum.empty?(tracks), do: [], else: [notify_parent: {:tracks, tracks}]

    {actions, state}
  end

  @impl true
  def handle_parent_notification({:candidate, candidate}, _ctx, state) do
    candidate = ICECandidate.from_json(candidate)
    :ok = PeerConnection.add_ice_candidate(state.pc, candidate)

    {[], state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    dbg(msg)
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

    actions =
      with {:ok, engine_track_id} <- Map.fetch(state.inbound_tracks, track_id),
           pad <- Pad.ref(:output, {engine_track_id, rid}),
           true <- Map.has_key?(ctx.pads, pad) do
        rtp = %{marker: packet.marker}

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
    {[], state}
  end

  defp handle_webrtc_msg({:rtcp, _packets}, _ctx, state) do
    {[], state}
  end

  defp handle_webrtc_msg(msg, _ctx, state) do
    dbg(msg)
    {[], state}
  end

  defp receive_new_tracks(), do: do_receive_new_tracks([])

  defp do_receive_new_tracks(acc) do
    receive do
      {:ex_webrtc, _pc, {:track, track}} -> do_receive_new_tracks([track | acc])
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
