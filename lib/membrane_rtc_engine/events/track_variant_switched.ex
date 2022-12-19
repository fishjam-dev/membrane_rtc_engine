defmodule Membrane.RTC.Engine.Event.TrackVariantSwitched do
  @moduledoc """
  Event sent whenever Engine starts sending a new track variant.

  See also `Membrane.RTC.Engine.Event.RequestTrackVariant`.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantSwitched event.

  * `new_variant` - variant that engine will be sending from now
  * `reason` - a reason passed in `Membrane.RTC.Engine.Event.RequestTrackVariant`.
  Defaults to `nil`,
  """
  @type t :: %__MODULE__{new_variant: Track.variant(), reason: atom()}

  @enforce_keys [:new_variant]
  defstruct @enforce_keys ++ [reason: nil]
end
