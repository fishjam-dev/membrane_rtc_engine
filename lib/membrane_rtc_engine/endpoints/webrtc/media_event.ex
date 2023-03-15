defmodule Membrane.RTC.Engine.Endpoint.WebRTC.MediaEvent do
  @moduledoc false

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.{Peer, Track}

  @type t() :: map()

  @spec peer_accepted(Peer.id(), list()) :: t()
  def peer_accepted(peer_id, peers) do
    %{type: "peerAccepted", data: %{id: peer_id, peersInRoom: peers}}
  end

  @spec peer_denied(map()) :: t()
  def peer_denied(metadata \\ %{}) do
    %{type: "peerDenied", data: metadata}
  end

  @spec peer_joined(Peer.t()) :: t()
  def peer_joined(%Peer{id: id, metadata: metadata}) do
    %{type: "peerJoined", data: %{peer: %{id: id, metadata: metadata}}}
  end

  @spec peer_left(Peer.id()) :: t()
  def peer_left(peer_id) do
    %{type: "peerLeft", data: %{peerId: peer_id}}
  end

  @spec peer_updated(Peer.t()) :: t()
  def peer_updated(peer) do
    %{type: "peerUpdated", data: %{peerId: peer.id, metadata: peer.metadata}}
  end

  @spec peer_removed(Peer.id(), String.t()) :: t()
  def peer_removed(peer_id, reason) do
    %{type: "peerRemoved", data: %{peerId: peer_id, reason: reason}}
  end

  @spec tracks_added(Peer.id(), map()) :: t()
  def tracks_added(peer_id, track_id_to_metadata) do
    %{type: "tracksAdded", data: %{peerId: peer_id, trackIdToMetadata: track_id_to_metadata}}
  end

  @spec tracks_removed(Peer.id(), [String.t()]) :: t()
  def tracks_removed(peer_id, track_ids) do
    %{type: "tracksRemoved", data: %{peerId: peer_id, trackIds: track_ids}}
  end

  @spec track_updated(Peer.id(), String.t(), map()) :: t()
  def track_updated(peer_id, track_id, metadata) do
    %{type: "trackUpdated", data: %{peerId: peer_id, trackId: track_id, metadata: metadata}}
  end

  @spec tracks_priority([String.t()]) :: t()
  def tracks_priority(tracks) do
    %{type: "tracksPriority", data: %{tracks: tracks}}
  end

  @spec encoding_switched(
          Peer.id(),
          Track.id(),
          String.t(),
          TrackReceiver.variant_switch_reason()
        ) :: t()
  def encoding_switched(peer_id, track_id, encoding, reason) do
    as_custom(%{
      type: "encodingSwitched",
      data: %{peerId: peer_id, trackId: track_id, encoding: encoding, reason: reason}
    })
  end

  @spec sdp_answer(String.t(), %{String.t() => non_neg_integer()}) :: t()
  def sdp_answer(answer, mid_to_track_id) do
    as_custom(%{
      type: "sdpAnswer",
      data: %{
        type: "answer",
        sdp: answer,
        midToTrackId: mid_to_track_id
      }
    })
  end

  @spec offer_data(%{audio: non_neg_integer(), video: non_neg_integer()}, turns: [map()]) :: t()
  def offer_data(tracks_types, turns) do
    integrated_turn_servers =
      Enum.map(turns, fn turn ->
        addr =
          if turn.relay_type == :tls and turn[:domain_name],
            do: turn[:domain_name],
            else: :inet.ntoa(turn.mocked_server_addr) |> to_string()

        %{
          serverAddr: addr,
          serverPort: turn.server_port,
          transport: turn.relay_type,
          password: turn.password,
          username: turn.username
        }
      end)

    as_custom(%{
      type: "offerData",
      data: %{
        tracksTypes: tracks_types,
        integratedTurnServers: integrated_turn_servers
      }
    })
  end

  @spec candidate(String.t(), non_neg_integer()) :: t()
  def candidate(candidate, sdp_m_line_index) do
    as_custom(%{
      type: "candidate",
      data: %{
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
      }
    })
  end

  @spec sdp_offer(String.t()) :: t()
  def sdp_offer(offer) do
    as_custom(%{
      type: "sdpOffer",
      data: %{
        type: "offer",
        sdp: offer
      }
    })
  end

  @spec voice_activity(Track.id(), :speech | :silence) :: t()
  def voice_activity(track_id, vad),
    do:
      as_custom(%{
        type: "vadNotification",
        data: %{
          trackId: track_id,
          status: vad
        }
      })

  @spec bandwidth_estimation(non_neg_integer()) :: t()
  def bandwidth_estimation(estimation),
    do:
      as_custom(%{
        type: "bandwidthEstimation",
        data: %{
          estimation: estimation
        }
      })

  @spec encode(t()) :: binary()
  def encode(event), do: Jason.encode!(event)

  @spec decode(binary()) :: {:ok, t()} | {:error, :invalid_media_event}
  def decode(event_json) do
    with {:ok, event} <- Jason.decode(event_json), do: do_decode(event)
  end

  defp do_decode(%{"type" => "join", "data" => %{"metadata" => metadata}}),
    do: {:ok, %{type: :join, data: %{metadata: metadata}}}

  defp do_decode(%{"type" => "leave"}), do: {:ok, %{type: :leave}}

  defp do_decode(%{"type" => "custom", "data" => data}) do
    with {:ok, event} <- decode_custom_media_event(data), do: {:ok, %{type: :custom, data: event}}
  end

  defp do_decode(%{"type" => "updatePeerMetadata", "data" => %{"metadata" => metadata}}),
    do: {:ok, %{type: :update_peer_metadata, data: %{metadata: metadata}}}

  defp do_decode(%{
         "type" => "updateTrackMetadata",
         "data" => %{"trackId" => track_id, "trackMetadata" => metadata}
       }),
       do:
         {:ok,
          %{type: :update_track_metadata, data: %{track_id: track_id, track_metadata: metadata}}}

  defp do_decode(_event), do: {:error, :invalid_media_event}

  defp decode_custom_media_event(%{"type" => "renegotiateTracks"}) do
    {:ok, %{type: :renegotiate_tracks}}
  end

  defp decode_custom_media_event(%{"type" => "prioritizeTrack"} = event) do
    case event do
      %{"type" => "prioritizeTrack", "data" => %{"trackId" => track_id}} ->
        {:ok, %{type: :prioritize_track, data: %{track_id: track_id}}}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "unprioritizeTrack"} = event) do
    case event do
      %{"type" => "unprioritizeTrack", "data" => %{"trackId" => track_id}} ->
        {:ok, %{type: :unprioritize_track, data: %{track_id: track_id}}}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "preferedVideoSizes"} = event) do
    case event do
      %{
        "type" => "preferedVideoSizes",
        "data" => %{
          "bigScreens" => big_screens,
          "mediumScreens" => medium_screens,
          "smallScreens" => small_screens,
          "allSameSize" => same_size?
        }
      } ->
        {:ok,
         %{
           type: :prefered_video_sizes,
           data: %{
             big_screens: big_screens,
             medium_screens: medium_screens,
             small_screens: small_screens,
             same_size?: same_size?
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "candidate"} = event) do
    case event do
      %{
        "type" => "candidate",
        "data" => %{
          "candidate" => candidate,
          "sdpMLineIndex" => sdp_m_line_index
        }
      } ->
        {:ok,
         %{
           type: :candidate,
           data: %{
             candidate: candidate,
             sdp_m_line_index: sdp_m_line_index
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "trackVariantBitrates"} = event) do
    case event do
      %{
        "type" => "trackVariantBitrates",
        "data" => %{
          "trackId" => track_id,
          "variantBitrates" => variant_bitrates
        }
      } ->
        {:ok,
         %{
           type: :track_variant_bitrates,
           data: %{
             track_id: track_id,
             variant_bitrates: variant_bitrates
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "sdpOffer"} = event) do
    case event do
      %{
        "type" => "sdpOffer",
        "data" => %{
          "sdpOffer" => %{
            "type" => "offer",
            "sdp" => sdp
          },
          "trackIdToTrackInfo" => track_id_to_track_info
        }
      } ->
        # max_bandwidth map still has rid strings as keys (e.g. %{"h" => 1500})
        decoded_track_id_to_track_info =
          track_id_to_track_info
          |> Enum.map(fn {id, track_info} ->
            {id,
             %{
               track_metadata: Map.get(track_info, "trackMetadata", :invalid),
               max_bandwidth: Map.get(track_info, "maxBandwidth", :invalid),
               mid: Map.get(track_info, "mid", :invalid)
             }}
          end)
          |> Map.new()

        is_valid =
          for(
            {_id, track_info} <- decoded_track_id_to_track_info,
            {_key, value} <- track_info,
            do: value != :invalid
          )
          |> Enum.all?()

        if is_valid do
          {:ok,
           %{
             type: :sdp_offer,
             data: %{
               sdp_offer: %{
                 type: :offer,
                 sdp: sdp
               },
               track_id_to_track_info: decoded_track_id_to_track_info
             }
           }}
        else
          {:error, :invalid_media_event}
        end

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(%{"type" => "setTargetTrackVariant"} = event) do
    case event do
      %{
        "type" => "setTargetTrackVariant",
        "data" => %{
          "trackId" => tid,
          "variant" => variant
        }
      } ->
        {:ok, %{type: :set_target_track_variant, data: %{track_id: tid, variant: variant}}}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp decode_custom_media_event(_event), do: {:error, :invalid_media_event}

  defp as_custom(msg) do
    %{type: "custom", data: msg}
  end
end
