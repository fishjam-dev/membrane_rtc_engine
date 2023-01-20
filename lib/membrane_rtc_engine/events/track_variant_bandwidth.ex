defmodule Membrane.RTC.Engine.Event.TrackVariantBandwidth do
  @moduledoc """
  Event indicating bandwidth of each of the track's variants.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantBandwidth event.
  """
  @type t :: %__MODULE__{variant: Track.variant(), bandwidth: non_neg_integer()}

  @enforce_keys [:variant, :bandwidth]
  defstruct @enforce_keys
end
