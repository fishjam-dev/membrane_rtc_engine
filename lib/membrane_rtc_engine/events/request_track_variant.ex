defmodule Membrane.RTC.Engine.Event.RequestTrackVariant do
  @moduledoc """
  Event for changing track variant that is currently being received.

  Engine will start sending a new track variant as soon
  as it receives a keyframe for the new variant.

  See also `Membrane.RTC.Engine.Event.TrackVariantSwitched`.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing RequestTrackVariant event.

  * `variant` - track variant to receive
  """
  @type t() :: %__MODULE__{variant: Track.variant()}

  @enforce_keys [:variant]
  defstruct @enforce_keys
end
