defmodule Membrane.RTP.VP8.Utils do
  @moduledoc """
  Utility functions for RTP packets containing VP8 encoded frames.
  """

  @doc """
  Checks whether RTP payload contains VP8 keyframe.
  """
  @spec is_keyframe(binary()) :: boolean()
  def is_keyframe(rtp_payload) do
    # RTP payload contains VP8 keyframe when P bit in VP8 payload header is set to 0
    # besides this S bit (start of VP8 partition) and PID (partition index)
    # have to be 1 and 0 respectively
    # for more information refer to RFC 7741 Sections 4.2 and 4.3

    with {:ok, {payload_descriptor, payload}} <-
           Membrane.RTP.VP8.PayloadDescriptor.parse_payload_descriptor(rtp_payload),
         <<_size0::3, _h::1, _ver::3, p::1, _size1::8, _size2::8, _rest::binary()>> <- payload do
      payload_descriptor.s == 1 and payload_descriptor.partition_index == 0 and p == 0
    else
      _err -> false
    end
  end

  @doc """
  Checks whether RTP payload contains new frame.
  """
  @spec is_new_frame(binary()) :: boolean()
  def is_new_frame(rtp_payload) do
    case Membrane.RTP.VP8.PayloadDescriptor.parse_payload_descriptor(rtp_payload) do
      {:ok, {payload_descriptor, _payload}} ->
        payload_descriptor.s == 1 and payload_descriptor.partition_index == 0

      _err ->
        false
    end
  end
end
