defmodule Membrane.RTC.Engine.Peer do
  @moduledoc false
  use Bunch.Access
  alias Membrane.RTC.Engine.Track

  @type id() :: any()

  @type t :: %__MODULE__{
          id: id(),
          metadata: any(),
          track_id_to_track_metadata: %{Track.id() => any()}
        }

  defstruct id: nil,
            metadata: nil,
            track_id_to_track_metadata: %{}

  @spec new(
          id :: id(),
          metadata :: any(),
          track_id_to_track_metadata :: %{Track.id() => any()}
        ) :: Peer.t()
  def new(id, metadata, track_id_to_track_metadata) do
    %__MODULE__{
      id: id,
      metadata: metadata,
      track_id_to_track_metadata: track_id_to_track_metadata
    }
  end
end
