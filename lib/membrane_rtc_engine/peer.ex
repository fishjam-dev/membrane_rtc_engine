defmodule Membrane.RTC.Engine.Peer do
  @moduledoc false
  use Bunch.Access

  @type id() :: any()

  @type t :: %__MODULE__{
          id: id(),
          metadata: any()
        }

  defstruct id: nil,
            metadata: nil

  @spec new(
          id :: id(),
          metadata :: any()
        ) :: Peer.t()
  def new(id, metadata) do
    %__MODULE__{
      id: id,
      metadata: metadata
    }
  end
end
