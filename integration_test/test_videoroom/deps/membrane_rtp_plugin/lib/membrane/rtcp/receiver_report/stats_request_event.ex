defmodule Membrane.RTCP.ReceiverReport.StatsRequestEvent do
  @moduledoc """
  Event to be sent to jitter buffer to request statistics.
  """
  @derive Membrane.EventProtocol
  defstruct []
  @type t :: %__MODULE__{}
end
