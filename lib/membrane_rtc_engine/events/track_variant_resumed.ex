defmodule Membrane.RTC.Engine.Event.TrackVariantResumed do
  @moduledoc """
  Event sent whenever track variant was resumed.
  """

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackVariantResumed event.
  """
  @type t :: %__MODULE__{}

  defstruct []
end
