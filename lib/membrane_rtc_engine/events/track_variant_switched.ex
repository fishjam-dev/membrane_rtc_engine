defmodule Membrane.RTC.Engine.Event.TrackVariantSwitched do
  @moduledoc """
  Event sent whenever Engine starts sending a new track variant.

  See also `Membrane.RTC.Engine.Event.SwitchTrackVariant`.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantSwitched event.

  * `new_variant` - variant that engine will be sending from now
  * `buffer` - first buffer of a new variant. It's always
  keyframe (or its beginning).
  """
  @type t :: %__MODULE__{
          new_variant: Track.variant(),
          buffer: Membrane.Buffer.t()
        }

  @enforce_keys [:new_variant, :buffer]
  defstruct @enforce_keys
end
