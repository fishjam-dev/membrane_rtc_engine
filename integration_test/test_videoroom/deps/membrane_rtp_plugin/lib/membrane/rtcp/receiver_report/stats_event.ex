defmodule Membrane.RTCP.ReceiverReport.StatsEvent do
  @moduledoc """
  Event carrying statistics for a receiver report.
  """

  @derive Membrane.EventProtocol
  @enforce_keys [:stats]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          stats: Membrane.RTCP.ReceiverReport.Stats.t()
        }
end
