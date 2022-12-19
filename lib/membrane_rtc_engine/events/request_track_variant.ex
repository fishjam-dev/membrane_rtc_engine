defmodule Membrane.RTC.Engine.Event.RequestTrackVariant do
  @moduledoc """
  Event for changing track variant that is currently being received.

  Sending this event will cause the engine to send a keyframe request
  event to the track origin. Once the engine receives a keyframe
  for the requested track variant, it will start forwarding it to
  the requester.

  If the requested track variant becomes inactive before being delivered
  it has to be re-requested when it becomes active again.

  See also `Membrane.RTC.Engine.Event.TrackVariantSwitched`.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing RequestTrackVariant event.

  * `variant` - track variant to receive
  * `reason` - reason of the request. It will be repeated in
  `Membrane.RTC.Engine.Event.TrackVariantSwitched`. Defaults to `nil`.
  """
  @type t() :: %__MODULE__{variant: Track.variant(), reason: atom()}

  @enforce_keys [:variant]
  defstruct @enforce_keys ++ [reason: nil]
end
