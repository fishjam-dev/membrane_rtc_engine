defmodule Membrane.RTC.Engine.Endpoint.WebRTC.MediaEvent do
  @moduledoc false

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Signalling.WebRTC.{ClientSignallingMsg, Payload, ServerSignallingMsg}
  alias Membrane.RTC.Engine.Signalling.WebRTC.Payload.SdpAnswer.MidToTrackIdEntry
  alias Membrane.RTC.Engine.{Peer, Track}

  @type t() :: ServerSignallingMsg.t()

  @spec peer_accepted(Peer.id()) :: t()
  def peer_accepted(peer_id) do
    %ServerSignallingMsg{content: {:peerAccepted, peer_id}}
  end

  @spec peer_joined(Peer.t()) :: t()
  def peer_joined(%Peer{} = peer) do
    peer = struct!(Payload.Peer, Map.from_struct(peer))
    %ServerSignallingMsg{content: {:peerJoined, peer}}
  end

  @spec peer_left(Peer.id()) :: t()
  def peer_left(peer_id) do
    %ServerSignallingMsg{content: {:peerLeft, peer_id}}
  end

  @spec peer_updated(Peer.t()) :: t()
  def peer_updated(%Peer{} = peer) do
    peer = struct!(Payload.Peer, Map.from_struct(peer))
    %ServerSignallingMsg{content: {:peerUpdated, peer}}
  end

  @spec track_added(Track.t()) :: t()
  def track_added(%Track{} = track) do
    track = %Payload.Track{
      trackId: track.id,
      metadata: track.metadata,
      owner: track.origin
    }

    %ServerSignallingMsg{content: {:trackAdded, track}}
  end

  @spec track_removed(Track.id()) :: t()
  def track_removed(track_id) do
    %ServerSignallingMsg{content: {:trackRemoved, track_id}}
  end

  @spec track_updated(Track.id(), map()) :: t()
  def track_updated(track_id, metadata) do
    track = %Payload.TrackWithMetadata{trackId: track_id, metadata: metadata}
    %ServerSignallingMsg{content: {:trackUpdated, track}}
  end

  @spec encoding_switched(
          Track.id(),
          String.t(),
          TrackReceiver.variant_switch_reason()
        ) :: t()
  def encoding_switched(track_id, encoding, reason) do
    track_variant = %Payload.TrackVariantSwitched{
      newVariant: %Payload.TrackVariant{
        trackId: track_id,
        variant: encoding
      },
      reason: Atom.to_string(reason)
    }

    %ServerSignallingMsg{content: {:variantSwitched, track_variant}}
  end

  @spec sdp_answer(Strint.t(), %{String.t() => Track.id()}) :: t()
  def sdp_answer(sdp, mid_to_track_id) do
    mid_to_track_id =
      Enum.map(mid_to_track_id, fn {key, value} -> %MidToTrackIdEntry{key: key, value: value} end)

    answer = %Payload.SdpAnswer{
      sdp: sdp,
      midToTrackId: mid_to_track_id
    }

    %ServerSignallingMsg{content: {:sdpAnswer, answer}}
  end

  @spec offer_data(%{audio: non_neg_integer(), video: non_neg_integer()}, turns: [map()]) :: t()
  def offer_data(tracks_types, turns) do
    integrated_turn_servers =
      Enum.map(turns, fn turn ->
        addr =
          if turn.relay_type == :tls and turn[:domain_name],
            do: turn[:domain_name],
            else: :inet.ntoa(turn.mocked_server_addr) |> to_string()

        %Payload.OfferData.TurnServer{
          addr: addr,
          port: turn.server_port,
          transport: Atom.to_string(turn.relay_type),
          password: turn.password,
          username: turn.username
        }
      end)

    payload = %Payload.OfferData{
      integratedTurnServers: integrated_turn_servers,
      audioTracks: tracks_types.audio,
      videoTracks: tracks_types.video
    }

    %ServerSignallingMsg{content: {:offerData, payload}}
  end

  @spec candidate(String.t(), non_neg_integer()) :: t()
  def candidate(candidate, sdp_m_line_index) do
    payload = %Payload.ICECandidate{
      candidate: candidate,
      sdpMLineIndex: sdp_m_line_index
    }

    %ServerSignallingMsg{content: {:candidate, payload}}
  end

  @spec sdp_offer(String.t()) :: t()
  def sdp_offer(sdp) do
    %ServerSignallingMsg{content: {:sdpOffer, %Payload.SdpOffer{sdp: sdp}}}
  end

  @spec voice_activity(Track.id(), :speech | :silence) :: t()
  def voice_activity(track_id, vad) do
    payload = %Payload.VoiceActivity{
      trackId: track_id,
      vad: vad
    }

    %ServerSignallingMsg{content: {:vadNotification, payload}}
  end

  @spec bandwidth_estimation(non_neg_integer()) :: t()
  def bandwidth_estimation(estimation) do
    %ServerSignallingMsg{content: {:bandwidthEstimation, estimation}}
  end

  @spec encode(t()) :: binary()
  def encode(%ServerSignallingMsg{} = event), do: ServerSignallingMsg.encode(event)

  @spec decode(binary()) :: {:ok, t()} | {:error, :invalid_media_event}
  def decode(binary_event) do
    {:ok, ClientSignallingMsg.decode(binary_event)}
  rescue
    Protobuf.DecodeError -> {:error, :invalid_media_event}
  end
end
