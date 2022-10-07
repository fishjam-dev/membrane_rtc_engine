defmodule Membrane.RTC.Engine.Event.TrackStopped do
  @moduledoc """
  Event for stopping track. When track stopped TrackReceiver will not receive any variant of track.

  To start forwarding track again send `Membrane.RTC.Engine.Event.RequestTrackVariant`.
  """

  @derive Membrane.EventProtocol

  @typedoc """
  Type describing TrackStopped event.
  """
  @type t() :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys
end
