defmodule Membrane.RTC.Engine.MediaEvent do
  @moduledoc false

  @type peer_id_t() :: String.t()
  @type to_t() :: peer_id_t() | :broadcast
  @type rtc_media_event_t() :: {:rtc_media_event, to_t(), binary()}

  @spec create_peer_accepted_event(peer_id_t(), map(), list(), boolean()) :: :: rtc_media_event_t()
  def create_peer_accepted_event(peer_id, peers, integrated_turn_servers, enforce_turns?) do
    peers =
      Enum.map(peers, fn {id, peer} ->
        %{id: id, metadata: peer.metadata, trackIdToMetadata: peer.track_id_to_track_metadata}
      end)

    integrated_turn_servers =
      Enum.map(integrated_turn_servers, fn turn ->
        addr = :inet.ntoa(turn.mocked_server_addr) |> to_string()

        %{
          serverAddr: addr,
          serverPort: turn.server_port,
          transport: turn.relay_type,
          password: turn.password,
          username: turn.username
        }
      end)

    ice_transport_policy = if enforce_turns?, do: "relay", else: "all"

    %{
      type: "peerAccepted",
      data: %{
        id: peer_id,
        peersInRoom: peers,
        integratedTurnServers: integrated_turn_servers,
        iceTransportPolicy: ice_transport_policy
      }
    }
    |> do_create(peer_id)
  end

  @spec create_peer_denied_event(peer_id_t(), map()) :: rtc_media_event_t()
  def create_peer_denied_event(peer_id, metadata \\ %{}) do
    %{type: "peerDenied", data: metadata}
    |> do_create(peer_id)
  end

  @spec create_peer_joined_event(peer_id_t(), map()) :: rtc_media_event_t()
  def create_peer_joined_event(peer_id, peer) do
    %{
      type: "peerJoined",
      data: %{
        peer: %{
          id: peer_id,
          metadata: peer.metadata
        }
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_peer_left_event(peer_id_t()) :: rtc_media_event_t()
  def create_peer_left_event(peer_id) do
    %{
      type: "peerLeft",
      data: %{
        peerId: peer_id
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_tracks_added_event(peer_id_t(), map()) ::
          rtc_media_event_t()
  def create_tracks_added_event(peer_id, track_id_to_metadata) do
    %{
      type: "tracksAdded",
      data: %{
        peerId: peer_id,
        trackIdToMetadata: track_id_to_metadata
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_tracks_removed_event(peer_id_t(), [String.t()]) ::
          rtc_media_event_t()
  def create_tracks_removed_event(peer_id, track_ids) do
    %{
      type: "tracksRemoved",
      data: %{
        peerId: peer_id,
        trackIds: track_ids
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_peer_updated_event(peer_id_t(), map()) :: rtc_media_event_t()
  def create_peer_updated_event(peer_id, peer) do
    %{
      type: "peerUpdated",
      data: %{
        peerId: peer_id,
        metadata: peer.metadata
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_track_updated_event(peer_id_t(), String.t(), map()) :: rtc_media_event_t()
  def create_track_updated_event(peer_id, track_id, metadata) do
    %{
      type: "trackUpdated",
      data: %{
        peerId: peer_id,
        trackId: track_id,
        metadata: metadata
      }
    }
    |> do_create(:broadcast)
  end

  @spec create_custom_event(peer_id_t(), map()) :: rtc_media_event_t()
  def create_custom_event(peer_id, msg) do
    %{
      type: "custom",
      data: msg
    }
    |> do_create(peer_id)
  end

  @spec create_error_event(to_t(), String.t()) :: rtc_media_event_t()
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
    |> then(fn event -> {:rtc_media_event, to, event} end)
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
