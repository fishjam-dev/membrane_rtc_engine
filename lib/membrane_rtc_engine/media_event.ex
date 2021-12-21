defmodule Membrane.RTC.Engine.MediaEvent do
  @moduledoc false

  alias Membrane.RTC.Engine.Endpoint
  alias Membrane.RTC.Engine.Peer

  @type rtc_media_event_t() :: binary()

  @spec create_peer_accepted_event(Peer.id(), map(), [Endpoint.t()]) :: rtc_media_event_t()
  def create_peer_accepted_event(peer_id, peers, endpoints) do
    peers =
      Enum.map(peers, fn {id, peer} ->
        track_id_to_track_metadata = Endpoint.get_track_id_to_metadata(endpoints[id])
        %{id: id, metadata: peer.metadata, trackIdToMetadata: track_id_to_track_metadata}
      end)

    %{
      type: "peerAccepted",
      data: %{
        id: peer_id,
        peersInRoom: peers
      }
    }
    |> serialize()
  end

  @spec create_peer_denied_event(map()) :: rtc_media_event_t()
  def create_peer_denied_event(metadata \\ %{}) do
    %{type: "peerDenied", data: metadata}
    |> serialize()
  end

  @spec create_peer_joined_event(Peer.t()) :: rtc_media_event_t()
  def create_peer_joined_event(peer) do
    %{
      type: "peerJoined",
      data: %{
        peer: %{
          id: peer.id,
          metadata: peer.metadata
        }
      }
    }
    |> serialize()
  end

  @spec create_peer_left_event(Peer.id()) :: rtc_media_event_t()
  def create_peer_left_event(peer_id) do
    %{
      type: "peerLeft",
      data: %{
        peerId: peer_id
      }
    }
    |> serialize()
  end

  @spec create_tracks_added_event(Peer.id(), map()) ::
          rtc_media_event_t()
  def create_tracks_added_event(peer_id, track_id_to_metadata) do
    %{
      type: "tracksAdded",
      data: %{
        peerId: peer_id,
        trackIdToMetadata: track_id_to_metadata
      }
    }
    |> serialize()
  end

  @spec create_tracks_removed_event(Peer.id(), [String.t()]) ::
          rtc_media_event_t()
  def create_tracks_removed_event(peer_id, track_ids) do
    %{
      type: "tracksRemoved",
      data: %{
        peerId: peer_id,
        trackIds: track_ids
      }
    }
    |> serialize()
  end

  @spec create_peer_updated_event(Peer.t()) :: rtc_media_event_t()
  def create_peer_updated_event(peer) do
    %{
      type: "peerUpdated",
      data: %{
        peerId: peer.id,
        metadata: peer.metadata
      }
    }
    |> serialize()
  end

  @spec create_track_updated_event(Peer.id(), String.t(), map()) :: rtc_media_event_t()
  def create_track_updated_event(peer_id, track_id, metadata) do
    %{
      type: "trackUpdated",
      data: %{
        peerId: peer_id,
        trackId: track_id,
        metadata: metadata
      }
    }
    |> serialize()
  end

  @spec create_custom_event(map()) :: rtc_media_event_t()
  def create_custom_event(msg) do
    %{
      type: "custom",
      data: msg
    }
    |> serialize()
  end

  @spec create_error_event(String.t()) :: rtc_media_event_t()
  def create_error_event(msg) do
    %{
      type: "error",
      data: %{
        message: msg
      }
    }
    |> serialize()
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
          "receiveMedia" => receive_media,
          "metadata" => metadata
        }
      } ->
        {:ok,
         %{
           type: :join,
           data: %{
             receive_media: receive_media,
             metadata: metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "custom", "data" => data}) do
    {:ok, %{type: :custom, data: data}}
  end

  defp do_deserialize(%{"type" => "updatePeerMetadata"} = event) do
    case event do
      %{
        "type" => "updatePeerMetadata",
        "data" => %{
          "metadata" => peer_metadata
        }
      } ->
        {:ok,
         %{
           type: :update_peer_metadata,
           data: %{
             metadata: peer_metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "updateTrackMetadata"} = event) do
    case event do
      %{
        "type" => "updateTrackMetadata",
        "data" => %{
          "trackId" => track_id,
          "trackMetadata" => track_metadata
        }
      } ->
        {:ok,
         %{
           type: :update_track_metadata,
           data: %{
             track_id: track_id,
             track_metadata: track_metadata
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp do_deserialize(%{"type" => "leave"}), do: {:ok, %{type: :leave}}

  defp do_deserialize(_event), do: {:error, :invalid_media_event}
end
