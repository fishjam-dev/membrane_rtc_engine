defmodule Membrane.RTCP.TransportFeedbackPacket.NACK do
  @moduledoc """
  Generic Negative Acknowledgment packet that informs about lost RTP packet(s)

  Quoting [RFC4585](https://datatracker.ietf.org/doc/html/rfc4585#section-6.2.1):
  The Generic NACK is used to indicate the loss of one or more RTP packets.
  The lost packet(s) are identified by the means of a packet identifier and a bit mask.

  The Feedback Control Information (FCI) field has the following Syntax (Figure 4):

  ```txt
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |            PID                |             BLP               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

               Figure 4: Syntax for the Generic NACK message

  Packet ID (PID): 16 bits
     The PID field is used to specify a lost packet.  The PID field
     refers to the RTP sequence number of the lost packet.


  bitmask of following lost packets (BLP): 16 bits
     The BLP allows for reporting losses of any of the 16 RTP packets
     immediately following the RTP packet indicated by the PID.  The
     BLP's definition is identical to that given in [6].  Denoting the
     BLP's least significant bit as bit 1, and its most significant bit
     as bit 16, then bit i of the bit mask is set to 1 if the receiver
     has not received RTP packet number (PID+i) (modulo 2^16) and
     indicates this packet is lost; bit i is set to 0 otherwise.  Note
     that the sender MUST NOT assume that a receiver has received a
     packet because its bit mask was set to 0.  For example, the least
     significant bit of the BLP would be set to 1 if the packet
     corresponding to the PID and the following packet have been lost.
     However, the sender cannot infer that packets PID+2 through PID+16
     have been received simply because bits 2 through 15 of the BLP are
     0; all the sender knows is that the receiver has not reported them
     as lost at this time.

  ```
  Implementation based on https://datatracker.ietf.org/doc/html/rfc4585#section-6.2.1
  and https://datatracker.ietf.org/doc/html/rfc2032#section-5.2.2
  """

  @behaviour Membrane.RTCP.TransportFeedbackPacket

  import Bitwise

  defstruct lost_packet_ids: []

  @impl true
  def decode(nack_fci) do
    for <<packet_id::unsigned-size(16), bit_mask::unsigned-size(16) <- nack_fci>> do
      next_lost_packets =
        0..15
        |> Enum.map(fn i ->
          if (bit_mask >>> i &&& 1) == 1 do
            mod_16bit(packet_id + i + 1)
          else
            nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      [packet_id | next_lost_packets]
    end
    |> then(&{:ok, %__MODULE__{lost_packet_ids: List.flatten(&1)}})
  end

  @impl true
  def encode(%__MODULE__{lost_packet_ids: lost_packet_ids}) do
    # TODO: This code does handle rollover, so 65_535 and 0 will be put in separate FCIs
    #       That's not optimal, but rare and still correctly reports the lost packets
    ids_to_encode = Enum.sort(lost_packet_ids)

    # code splitting ids into tuples with reference_id and a list of following ids
    # greater by at most 16. They will fit into one FCI.
    chunk_fun = fn
      # Initial step - initilaize reference_id
      id, nil ->
        {:cont, {id, []}}

      # Id to group in the same FCI - no chunk emitted
      id, {reference_id, rest} when id > reference_id and id - reference_id <= 16 ->
        {:cont, {reference_id, [id | rest]}}

      # Id that should start a next FCI - emit a chunk and set a new reference_id
      id, {reference_id, rest} when id - reference_id > 16 ->
        {:cont, {reference_id, rest}, {id, []}}
    end

    # Emit a chunk with what has been gathered
    after_fun = fn acc -> {:cont, acc, nil} end

    ids_to_encode
    |> Enum.chunk_while(nil, chunk_fun, after_fun)
    |> Enum.map_join(&encode_fci/1)
  end

  defp encode_fci({reference_id, ids}) when is_integer(reference_id) do
    bit_mask =
      ids
      |> Enum.reduce(0, fn id, acc ->
        # ID must be between reference_id + 1 and reference_id + 16
        # we set bit 0 for reference_id + 1 and 15 for reference_id + 16
        bit_to_set = id - reference_id - 1
        bor(acc, 1 <<< bit_to_set)
      end)

    <<reference_id::unsigned-size(16), bit_mask::unsigned-size(16)>>
  end

  defp mod_16bit(number), do: number &&& 0xFFFF
end
