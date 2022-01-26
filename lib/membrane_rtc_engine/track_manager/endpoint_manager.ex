defmodule Membrane.RTC.Engine.EndpointManager do
  @moduledoc false

  alias Membrane.RTC.Engine.Track

  @enforce_keys [:id, :video_tracks_limit]
  defstruct @enforce_keys ++
              [
                inbound_tracks: %{},
                outbound_tracks: %{},
                prioritized_tracks: [],
                screens_sizes: %{same_size?: true, big_screens: nil, small_screens: nil}
              ]

  @type id :: String.t()

  @type t :: %__MODULE__{
          id: id(),
          inbound_tracks: %{Track.id() => Track.t()},
          outbound_tracks: %{Track.id() => Track.t()},
          video_tracks_limit: integer() | nil,
          prioritized_tracks: [String.t()],
          screens_sizes: %{
            same_size?: boolean(),
            big_screens: nil | integer(),
            medium_screens: nil | integer(),
            small_screens: nil | integer()
          }
        }

  @doc """
  Creates a new EndpointManager.
  """
  @spec new(
          endpoint_id :: id(),
          video_tracks_limit :: integer()
        ) :: t
  def new(endpoint_id, video_tracks_limit) do
    %__MODULE__{
      id: endpoint_id,
      video_tracks_limit: video_tracks_limit
    }
  end

  @spec add_tracks(
          endpoint :: t(),
          tracks :: [Track.t()],
          type :: :inbound_tracks | :outbound_tracks
        ) :: t()
  def add_tracks(endpoint, tracks, type) do
    track_id_to_track = Map.fetch!(endpoint, type)
    track_id_to_track = update_tracks(tracks, track_id_to_track)
    Map.put(endpoint, type, track_id_to_track)
  end

  @spec remove_tracks(
          endpoint :: t(),
          tracks :: [Track.t()],
          type :: :inbound_tracks | :outbound_tracks
        ) :: t()
  def remove_tracks(endpoint, tracks, type) do
    track_id_to_track = Map.fetch!(endpoint, type)

    track_id_to_track =
      Enum.reduce(tracks, track_id_to_track, fn track, acc -> Map.pop(acc, track.id) end)

    Map.put(endpoint, type, track_id_to_track)
  end

  @spec map_audio_to_video(endpoint_manager :: t(), track_id :: Track.id()) :: [Track.t()]
  def map_audio_to_video(endpoint_manager, track_id) do
    if Map.has_key?(endpoint_manager.inbound_tracks, track_id) do
      endpoint_manager.inbound_tracks
      |> Map.values()
      |> Enum.filter(&(&1.type == :video))
    else
      []
    end
  end

  @spec calculate_track_priorities(endpoint :: t(), ordered_tracks :: [Track.id()]) :: [
          Track.id()
        ]
  def calculate_track_priorities(endpoint, ordered_tracks) do
    ordered_tracks =
      Enum.reject(ordered_tracks, fn track ->
        Map.has_key?(endpoint.inbound_tracks, track.id) or track.id in endpoint.prioritized_tracks
      end)

    video_tracks_limit =
      if endpoint.video_tracks_limit == nil do
        endpoint.outbound_tracks |> Map.values() |> Enum.count()
      else
        max(endpoint.video_tracks_limit - Enum.count(endpoint.prioritized_tracks), 0)
      end

    ordered_tracks = ordered_tracks |> Enum.take(video_tracks_limit) |> Enum.map(& &1.id)

    endpoint.prioritized_tracks ++ ordered_tracks
  end

  @spec prioritize_track(endpoint :: t(), track_id :: Track.id()) :: t()
  def prioritize_track(endpoint, track_id) do
    %{endpoint | prioritized_tracks: [track_id | endpoint.prioritized_tracks]}
  end

  @spec unprioritize_track(endpoint :: t(), track_id :: Track.id()) :: t()
  def unprioritize_track(endpoint, track_id) do
    %{endpoint | prioritized_tracks: Enum.reject(endpoint.prioritized_tracks, &(&1 == track_id))}
  end

  @spec get_inbound_video_tracks(endpoint :: t()) :: [Track.t()]
  def get_inbound_video_tracks(nil) do
    []
  end

  def get_inbound_video_tracks(endpoint) do
    endpoint.inbound_tracks |> Map.values() |> Enum.filter(&(&1.type == :video))
  end

  defp update_tracks(tracks, track_id_to_track) do
    Enum.reduce(tracks, track_id_to_track, fn track, acc ->
      Map.put(acc, track.id, track)
    end)
  end
end
