defmodule Membrane.RTC.Engine.MediaEvent do
  @moduledoc false

  @type peer_id_t() :: String.t()
  @type to_t() :: peer_id_t() | :broadcast
  @type sfu_media_event_t() :: {:sfu_media_event, to_t(), binary()}

  @spec create_peer_accepted_event(peer_id_t(), map()) :: sfu_media_event_t()
  def create_peer_accepted_event(peer_id, peers) do
    peers =
      Enum.map(peers, fn {id, peer} ->
        %{id: id, metadata: peer.metadata, midToTrackMetadata: peer.mid_to_track_metadata}
      end)

    %{type: "peerAccepted", data: %{id: peer_id, peersInRoom: peers}}
    |> do_create(peer_id)
  end

  @spec create_peer_denied_event(peer_id_t(), map()) :: sfu_media_event_t()
  def create_peer_denied_event(peer_id, metadata \\ %{}) do
    %{type: "peerDenied", data: metadata}
    |> do_create(peer_id)
  end

  @spec create_peer_joined_event(peer_id_t(), map(), map()) :: sfu_media_event_t()
  def create_peer_joined_event(peer_id, metadata, mid_to_track_metadata) do
    %{
      type: "peerJoined",
      data: %{
        peer: %{
          id: peer_id,
          metadata: metadata,
          midToTrackMetadata: mid_to_track_metadata
        }
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_peer_left_event(peer_id_t()) :: sfu_media_event_t()
  def create_peer_left_event(peer_id) do
    %{
      type: "peerLeft",
      data: %{
        peerId: peer_id
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_signal_event(peer_id_t(), {:signal, {:candidate, String.t(), non_neg_integer()}}) ::
          sfu_media_event_t()
  def create_signal_event(peer_id, {:signal, {:candidate, candidate, sdp_m_line_index}}) do
    %{
      type: "candidate",
      data: %{
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
      }
    }
    |> do_create(peer_id)
  end

  @spec create_signal_event(peer_id_t(), {:signal, {:sdp_offer, String.t()}}) ::
          sfu_media_event_t()
  def create_signal_event(peer_id, {:signal, {:sdp_offer, offer}}) do
    %{
      type: "sdpOffer",
      data: %{
        type: "offer",
        sdp: offer
      }
    }
    |> do_create(peer_id)
  end

  @spec create_signal_event(peer_id_t(), {:signal, {:sdp_answer, String.t()}}) ::
          sfu_media_event_t()
  def create_signal_event(peer_id, {:signal, {:sdp_answer, answer}}) do
    %{
      type: "sdpAnswer",
      data: %{
        type: "answer",
        sdp: answer
      }
    }
    |> do_create(peer_id)
  end

  @spec create_error_event(to_t(), String.t()) :: sfu_media_event_t()
  def create_error_event(to, msg) do
    %{
      type: "error",
      data: %{
        message: msg
      }
    }
    |> do_create(to)
  end

  defp do_create(event, to) do
    event
    |> serialize()
    |> then(fn event -> {:sfu_media_event, to, event} end)
  end

  @spec serialize(map()) :: binary()
  def serialize(event), do: Jason.encode!(event)

  @spec deserialize(binary()) :: {:ok, map()} | {:error, :invalid_media_event}
  def deserialize(raw_event) do
    case Jason.decode(raw_event) do
      {:ok, event} -> do_deserialize(event)
      _error -> {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "join"} = event) do
    case event do
      %{
        "type" => "join",
        "data" => %{
          "relayAudio" => relay_audio,
          "relayVideo" => relay_video,
          "receiveMedia" => receive_media,
          "metadata" => metadata,
          "tracksMetadata" => tracks_metadata
        }
      } ->
        {:ok,
         %{
           type: :join,
           data: %{
             relay_audio: relay_audio,
             relay_video: relay_video,
             receive_media: receive_media,
             metadata: metadata,
             tracks_metadata: tracks_metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "sdpAnswer"} = event) do
    case event do
      %{
        "type" => "sdpAnswer",
        "data" => %{
          "sdpAnswer" => %{
            "type" => "answer",
            "sdp" => sdp
          },
          "midToTrackMetadata" => mid_to_track_metadata
        }
      } ->
        {:ok,
         %{
           type: :sdp_answer,
           data: %{
             sdp_answer: %{
               type: :answer,
               sdp: sdp
             },
             mid_to_track_metadata: mid_to_track_metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "sdpOffer"} = event) do
    case event do
      %{
        "type" => "sdpOffer",
        "data" => %{
          "sdpOffer" => %{
            "type" => "offer",
            "sdp" => sdp
          },
          "midToTrackMetadata" => mid_to_track_metadata
        }
      } ->
        {:ok,
         %{
           type: :sdp_offer,
           data: %{
             sdp_offer: %{
               type: :offer,
               sdp: sdp
             },
             mid_to_track_metadata: mid_to_track_metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "candidate"} = event) do
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

  defp do_deserialize(%{"type" => "leave"}), do: {:ok, %{type: :leave}}

  defp do_deserialize(_event), do: {:error, :invalid_media_event}
end
