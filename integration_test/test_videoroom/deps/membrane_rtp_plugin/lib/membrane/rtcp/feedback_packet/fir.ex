defmodule Membrane.RTCP.FeedbackPacket.FIR do
  @moduledoc """
  Encodes and decodes [Full Intra Request](https://datatracker.ietf.org/doc/html/rfc5104#section-4.3.1) packets.
  """
  @behaviour Membrane.RTCP.FeedbackPacket

  @enforce_keys [:target_ssrc, :seq_num]
  defstruct @enforce_keys

  @impl true
  def encode(%__MODULE__{} = packet) do
    <<packet.target_ssrc::32, packet.seq_num, 0::24>>
  end

  @impl true
  def encode(packets) when is_list(packets) do
    Enum.map_join(packets, &encode/1)
  end

  @impl true
  def decode(binary) do
    do_decode(binary, [])
  end

  defp do_decode(<<>>, acc) do
    {:ok, Enum.reverse(acc)}
  end

  defp do_decode(<<target_ssrc::32, seq_num, 0::24, rest::binary>>, acc) do
    do_decode(rest, [%__MODULE__{target_ssrc: target_ssrc, seq_num: seq_num} | acc])
  end

  defp do_decode(_binary, _acc) do
    {:error, :malformed_packet}
  end
end
