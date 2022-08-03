defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder.EncodingReport do
  @moduledoc """
  Struct representing a report about encodings returned by the Forwarder.

  It contains information about:
  - currently active encoding
  - an encoding that's waiting for a keyframe and is queued to become an active encoding
  """

  @enforce_keys [:currently_playing]
  defstruct @enforce_keys ++ [awaiting_keyframe: nil]

  @type t() :: %__MODULE__{
          currently_playing: String.t(),
          awaiting_keyframe: String.t() | nil
        }
end
