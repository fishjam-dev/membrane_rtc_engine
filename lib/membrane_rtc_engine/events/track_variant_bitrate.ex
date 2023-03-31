defmodule Membrane.RTC.Engine.Event.TrackVariantBitrate do
  @moduledoc """
  Event indicating bitrate of the track's variant.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantBitrate event.
  """
  @type t :: %__MODULE__{variant: Track.variant(), bitrate: non_neg_integer()}

  @enforce_keys [:variant, :bitrate]
  defstruct @enforce_keys
end
