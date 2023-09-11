defmodule Membrane.RTP.VadEvent do
  @moduledoc """
  An event informing about Voice Activity Detection status changes
  """

  @derive Membrane.EventProtocol
  @enforce_keys [:vad]
  defstruct @enforce_keys

  @typedoc """
  Type describing the structure of the Voice Activity Detection event.

  - `:vad` - contains information about VAD status. Indicates either speech or silence.
    For details on voice activity detection algorithm, refer to `Membrane.RTP.VAD`
  """
  @type t() :: %__MODULE__{
          vad: :speech | :silence
        }
end
