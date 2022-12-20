defmodule Membrane.RTC.Engine.MediaEvent do
  @moduledoc false

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

  @spec encoding_switched(Peer.id(), Track.id(), String.t()) :: t()
  def encoding_switched(peer_id, track_id, encoding) do
    %{type: "encodingSwitched", data: %{peerId: peer_id, trackId: track_id, encoding: encoding}}
  end

  @spec custom(map()) :: t()
  def custom(msg) do
    %{type: "custom", data: msg}
  end

  @spec create_error_event(String.t()) :: t()
  def create_error_event(msg) do
    %{type: "error", data: %{message: msg}}
  end

  @spec encode(t()) :: binary()
  def encode(event), do: Jason.encode!(event)

  @spec decode(binary()) :: {:ok, t()} | {:error, :invalid_media_event}
  def decode(event_json) do
    with {:ok, event_map} <- Jason.decode(event_json),
         event_type = Map.get(event_map, "type"),
         event_data = Map.get(event_map, "data"),
         %{} = event <- decode(event_type, event_data) do
      {:ok, event}
    else
      _error -> {:error, :invalid_media_event}
    end
  end

  defp decode(event_type, event_data)
  defp decode("join", %{"metadata" => metadata}), do: %{type: :join, data: %{metadata: metadata}}
  defp decode("leave", _event_data), do: %{type: :leave}
  defp decode("custom", data), do: %{type: :custom, data: data}

  defp decode("updatePeerMetadata", %{"metadata" => metadata}),
    do: %{type: :update_peer_metadata, data: %{metadata: metadata}}

  defp decode("updateTrackMetadata", %{"trackId" => track_id, "trackMetadata" => metadata}),
    do: %{type: :update_track_metadata, data: %{track_id: track_id, track_metadata: metadata}}

  defp decode(_event_type, _event_data), do: :error
end
