defmodule Membrane.RTP.PacketsDiscardedEvent do
  @moduledoc """
  Event carrying information about how many packets has been discarded by some element.
  """
  @derive Membrane.EventProtocol

  defstruct discarded: 0

  @type t :: %__MODULE__{
          discarded: non_neg_integer()
        }
end
