defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.ConnectionAllocator.AllocationGrantedNotification do
  @moduledoc """
  Message sent by ConnectionProber to TrackReceiver whenever new allocation is granted
  """

  @enforce_keys [:allocation]
  defstruct @enforce_keys

  @type t() :: %__MODULE__{allocation: non_neg_integer()}
end
