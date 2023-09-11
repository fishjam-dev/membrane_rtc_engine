defmodule Membrane.RTCPEvent do
  @moduledoc """
  Event carrying a parsed RTCP packet.
  """
  @derive Membrane.EventProtocol

  @enforce_keys [:rtcp]

  defstruct @enforce_keys ++ [ssrcs: [], arrival_timestamp: nil]

  @type t :: %__MODULE__{
          rtcp: Membrane.RTCP.Packet.t(),
          ssrcs: [Membrane.RTP.ssrc_t()],
          arrival_timestamp: Membrane.Time.t()
        }
end
