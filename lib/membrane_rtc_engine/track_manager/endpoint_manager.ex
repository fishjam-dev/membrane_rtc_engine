defmodule Membrane.RTC.Engine.EndpointManager do
  @moduledoc false

  alias Membrane.RTC.Engine.Track

  @enforce_keys [:endpoint_id, :video_tracks_limit]
  defstruct @enforce_keys ++ [inbound_tracks: %{}, outbound_tracks: %{}, prioritized_tracks: []]

  @type id :: String.t()

  @type t :: %__MODULE__{
          endpoint_id: id(),
          inbound_tracks: %{Track.id() => Track.t()},
          outbound_tracks: %{Track.id() => Track.t()},
          video_tracks_limit: integer(),
          prioritized_tracks: [String.t()]
        }

  @doc """
  Creates a new EndpointManager.
  """
  @spec new(
          endpoint_id: id(),
          video_tracks_limit: integer()
        ) :: t
  def new(endpoint_id, opts \\ []) do
    %__MODULE__{
      endpoint_id: endpoint_id,
      video_tracks_limit: Keyword.get(opts, :video_tracks_limit, 1)
    }
  end
end
