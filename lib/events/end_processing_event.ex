defmodule Membrane.RTC.Engine.Event.EndProcessing do
  @moduledoc """
  Event for informing endoint that engine end processing his track.
  """

  alias Membrane.RTC.Engine.Track

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing EndProcessing event.
  """
  @type t() :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys
end
