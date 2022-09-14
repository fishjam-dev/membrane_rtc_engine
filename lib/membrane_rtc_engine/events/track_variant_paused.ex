defmodule Membrane.RTC.Engine.Event.TrackVariantPaused do
  @moduledoc """
  Event sent whenever track variant was paused.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantPaused event.
  """
  @type t :: %__MODULE__{variant: Track.variant()}

  @enforce_keys [:variant]
  defstruct @enforce_keys
end
