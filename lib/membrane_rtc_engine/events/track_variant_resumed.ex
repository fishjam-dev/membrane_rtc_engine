defmodule Membrane.RTC.Engine.Event.TrackVariantResumed do
  @moduledoc """
  Event sent whenever track variant was resumed.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantResumed event.
  """
  @type t :: %__MODULE__{variant: Track.variant()}

  @enforce_keys [:variant]
  defstruct @enforce_keys
end
