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
  """
  @type t :: %__MODULE__{new_variant: Track.variant()}

  @enforce_keys [:new_variant]
  defstruct @enforce_keys
end
