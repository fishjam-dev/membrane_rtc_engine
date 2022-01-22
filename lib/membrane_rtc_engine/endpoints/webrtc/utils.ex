defmodule Membrane.RTC.Engine.Endpoint.WebRTC.Utils do
  @moduledoc false

  alias Membrane.RTP.VP8

  @spec is_vp8_keyframe(binary()) :: boolean()
  def is_vp8_keyframe(rtp_payload) do
    # RTP payload contains VP8 keyframe when P bit in VP8 payload header is set to 0
    # refer to https://datatracker.ietf.org/doc/html/rfc7741#section-4.3
    {:ok, {_payload_descriptor, payload}} =
      VP8.PayloadDescriptor.parse_payload_descriptor(rtp_payload)

    <<_size0::3, _h::1, _ver::3, p::1, _size1::8, _size2::8, _rest::binary()>> = payload
    p == 0
  end
end
