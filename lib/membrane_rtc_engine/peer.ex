defmodule Membrane.RTC.Engine.Peer do
  @moduledoc """
  Module describing Peer.
  """
  use Bunch.Access

  @type id() :: any()

  @type t :: %__MODULE__{
          id: id(),
          metadata: any()
        }

  @enforce_keys [:id]
  defstruct @enforce_keys ++ [:metadata]

  @spec new(id :: id(), metadata :: any()) :: t()
  def new(id, metadata) do
    %__MODULE__{
      id: id,
      metadata: metadata
    }
  end
end
