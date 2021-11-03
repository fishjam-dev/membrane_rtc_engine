defmodule Membrane.RTC.Engine.Endpoint do
  @moduledoc """
  Module representing a WebRTC connection.
  """
  use Bunch.Access
  alias Membrane.RTC.Engine.Track

  @type id() :: any()
  @type type() :: :screensharing | :participant

  @type t :: %__MODULE__{
          id: id(),
          type: :participant | :screensharing,
          inbound_tracks: %{Track.id() => Track.t()},
          ctx: any()
        }

  defstruct id: nil, type: :participant, inbound_tracks: %{}, ctx: nil

  @spec new(id :: id(), type :: type(), inbound_tracks :: [Track.t()], ctx :: any()) ::
          Endpoint.t()
  def new(id, type, inbound_tracks, ctx) do
    inbound_tracks = Map.new(inbound_tracks, &{&1.id, &1})
    %__MODULE__{id: id, type: type, inbound_tracks: inbound_tracks, ctx: ctx}
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

  @spec get_context(endpoint :: t()) :: any()
  def get_context(endpoint), do: endpoint.ctx

  @spec put_context(endpoint :: t(), ctx :: any()) :: Endpoint.t()
  def put_context(endpoint, ctx), do: %__MODULE__{endpoint | ctx: ctx}

  @spec update_track_encoding(endpoint :: Endpoint.t(), track_id :: Track.id(), encoding :: atom) ::
          Endpoint.t()
  def update_track_encoding(endpoint, track_id, value),
    do: update_in(endpoint.inbound_tracks[track_id], &%Track{&1 | encoding: value})
end
