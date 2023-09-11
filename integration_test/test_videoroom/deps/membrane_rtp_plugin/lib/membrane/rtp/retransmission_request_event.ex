defmodule Membrane.RTP.RetransmissionRequestEvent do
  @moduledoc """
  An event used to request a retransmission of packet(s).

  In the element responsible for sending NACKs it will be turned into
  `Membrane.RTCP.TransportFeedbackPacket.NACK` and sent to the RTP sender
  """
  @derive Membrane.EventProtocol
  @enforce_keys [:packet_ids]
  defstruct @enforce_keys
end
