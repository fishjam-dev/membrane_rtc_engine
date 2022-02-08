defmodule Membrane.RTC.Engine.Endpoint do
  @moduledoc false
  use Bunch.Access
  alias Membrane.RTC.Engine.Track

  @type id() :: any()

  @type t :: %__MODULE__{
          id: id(),
          inbound_tracks: %{Track.id() => Track.t()},
          state: :created | :removed
        }

  defstruct id: nil, inbound_tracks: %{}, state: :created

  @spec new(id :: id(), inbound_tracks :: [Track.t()]) :: t()
  def new(id, inbound_tracks) do
    inbound_tracks = Map.new(inbound_tracks, &{&1.id, &1})
    %__MODULE__{id: id, inbound_tracks: inbound_tracks}
  end

  @spec get_audio_tracks(endpoint :: t()) :: [Track.t()]
  def get_audio_tracks(endpoint),
    do: Map.values(endpoint.inbound_tracks) |> Enum.filter(&(&1.type == :audio))

  @spec get_video_tracks(endpoint :: t()) :: [Track.t()]
  def get_video_tracks(endpoint),
    do: Map.values(endpoint.inbound_tracks) |> Enum.filter(&(&1.type == :video))

  @spec get_track_by_id(endpoint :: t(), id :: Track.id()) :: Track.t() | nil
  def get_track_by_id(endpoint, id), do: endpoint.inbound_tracks[id]

  @spec get_tracks(endpoint :: t()) :: [Track.t()]
  def get_tracks(endpoint), do: Map.values(endpoint.inbound_tracks)

  @spec update_track_metadata(endpoint :: t(), track_id :: Track.id(), metadata :: any()) :: t()
  def update_track_metadata(endpoint, track_id, metadata),
    do:
      update_in(endpoint, [:inbound_tracks, track_id, :metadata], fn _old_metadata -> metadata end)

  @spec get_active_track_metadata(endpoint :: t()) :: %{Track.id() => any}
  def get_active_track_metadata(%__MODULE__{} = endpoint),
    do:
      endpoint.inbound_tracks
      |> Map.values()
      |> Enum.filter(& &1.active?)
      |> Map.new(&{&1.id, &1.metadata})

  def get_active_track_metadata(_endpoint), do: %{}

  @spec update_track_encoding(endpoint :: t(), track_id :: Track.id(), encoding :: atom) :: t()
  def update_track_encoding(endpoint, track_id, value),
    do: update_in(endpoint.inbound_tracks[track_id], &%Track{&1 | encoding: value})
end
