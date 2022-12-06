defmodule Membrane.RTC.Engine.Event.VoiceActivityChanged do
  @moduledoc """
  An event indicating that the voice activity on the given track has changed.

  Endpoints should only emit those events when they implement voice activity detection.
  """

  @derive Membrane.EventProtocol
  @enforce_keys [:voice_activity]
  defstruct @enforce_keys

  @typedoc """
  Type describing an event indicating changes in voice activity on the
  given audio track.
  """
  @type t() :: %__MODULE__{
          voice_activity: :speech | :silence
        }
end
