defmodule Membrane.RTC.Engine.Event.SwitchTrackVariant do
  @moduledoc """
  Event for changing track variant that is currently being received.

  Engine will start sending a new track variant as soon
  as it receives a keyframe for the new variant.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing SwitchTrackVariant event.

  * `new_variant` - track variant to receive
  """
  @type t() :: %__MODULE__{
          new_variant: Track.variant()
        }

  @enforce_keys [:new_variant]
  defstruct @enforce_keys
end
