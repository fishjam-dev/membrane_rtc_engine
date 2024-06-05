defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.PeerConnectionHandler do
  @moduledoc false
  use Membrane.Endpoint

  alias Membrane.RTC.Engine.Track

  alias ExWebRTC.{
    ICECandidate,
    MediaStreamTrack,
    RTPCodecParameters,
    RTPReceiver,
    RTPTransceiver,
    SessionDescription,
    PeerConnection
  }

  def_options(
    endpoint_id: [
      spec: String.t(),
      description: "Id of the parent endpoint"
    ]
  )

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

  @impl true
  def handle_init(_ctx, opts) do
    %{endpoint_id: endpoint_id} = opts

    {:ok, pc} =
      PeerConnection.start_link(
        ice_servers: @ice_servers,
        audio_codecs: @audio_codecs,
        video_codecs: @video_codecs
      )

    state = %{
      pc: pc,
      endpoint_id: endpoint_id,
      outbound_tracks: %{},
      inbound_tracks: %{}
    }

    {[], state}
  end

  @impl true
  def handle_pad_added(_pad, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_buffer(_pad, _buffer, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:offer, offer}, _ctx, state) do
    offer = SessionDescription.from_json(offer)
    :ok = PeerConnection.set_remote_description(state.pc, offer)

    {:ok, answer} = PeerConnection.create_answer(state.pc)
    :ok = PeerConnection.set_local_description(state.pc, answer)

    {tracks, state} =
      receive_new_tracks()
      |> make_tracks(state)

    answer = {:answer, SessionDescription.to_json(answer)}
    tracks = {:tracks, tracks}
    {[notify_parent: answer, notify_parent: tracks], state}
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
  def handle_info({:ex_webrtc, _from, msg}, _ctx, state) do
    handle_webrtc_msg(msg, state)
  end

  defp handle_webrtc_msg({:ice_candidate, candidate}, state) do
    msg = {:candidate, ICECandidate.to_json(candidate)}
    {[notify_parent: msg], state}
  end

  defp handle_webrtc_msg({:track, _track}, state) do
    raise("We do not expect to receive any tracks")
    {[], state}
  end

  defp handle_webrtc_msg({:connection_state_change, :connected}, state) do
    # TODO
    {[], state}
  end

  defp handle_webrtc_msg(_msg, state) do
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
