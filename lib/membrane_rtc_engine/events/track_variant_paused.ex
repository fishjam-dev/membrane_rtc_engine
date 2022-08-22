defmodule Membrane.RTC.Engine.Event.TrackVariantPaused do
  @moduledoc """
  Event sent whenever track variant was paused.
  """

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantPaused event.
  """
  @type t :: %__MODULE__{}

  defstruct []
end
