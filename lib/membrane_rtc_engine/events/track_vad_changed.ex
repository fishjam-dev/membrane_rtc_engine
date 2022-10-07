defmodule Membrane.RTC.Engine.Event.TrackVadChanged do
  @moduledoc """
  Event sent whenever audio track starts or stops speaking.
  """

  @derive Membrane.EventProtocol

  @type vad_value_t :: :speech | :silence

  @typedoc """
  Type describing TrackVadChanged event.

  * `vad_value` - information whether track represent silence or speech
  * `timestamp` - timestamp of when endpoint receives information about starts/stops speaking
  """
  @type t :: %__MODULE__{vad_value: vad_value_t(), timestamp: Membrane.Time.t()}

  @enforce_keys [:vad_value, :timestamp]
  defstruct @enforce_keys
end
