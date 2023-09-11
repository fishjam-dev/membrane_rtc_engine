defmodule Membrane.RTCP.ReceiverReport.Stats do
  @moduledoc """
  JitterBuffer stats that can be used for Receiver report generation
  """

  @enforce_keys [:fraction_lost, :total_lost, :highest_seq_num, :interarrival_jitter]

  defstruct @enforce_keys

  @type t ::
          %__MODULE__{
            fraction_lost: float(),
            total_lost: non_neg_integer(),
            highest_seq_num: Membrane.RTP.JitterBuffer.packet_index(),
            interarrival_jitter: non_neg_integer()
          }
          | :no_stats
end
