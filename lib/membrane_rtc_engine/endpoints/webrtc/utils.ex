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

  # This is rewritten from galene
  # https://github.com/jech/galene/blob/6fbdf0eab2c9640e673d9f9ec0331da24cbf2c4c/codecs/codecs.go#L119
  # and based on https://datatracker.ietf.org/doc/html/rfc6184#section-5
  @spec is_h264_keyframe(binary()) :: boolean()
  def is_h264_keyframe(rtp_payload) when byte_size(rtp_payload) < 1, do: false

  def is_h264_keyframe(rtp_payload) do
    <<_f::1, _nri::2, nalu_type::5, rest::binary()>> = rtp_payload
    do_is_h264_keyframe(nalu_type, rest)
  end

  # reserved
  defp do_is_h264_keyframe(0, _payload), do: false

  # single NALU
  defp do_is_h264_keyframe(nalu_type, _nalu) when nalu_type in [1..23] do
    nalu_type == 5
  end

  # STAP-A
  defp do_is_h264_keyframe(24, aus) do
    # aus - aggregation units
    # those might be single-time or multi-time aggreagtion units
    check_aggregation_units(24, aus)
  end

  # STAP-B, MTAP16 or MTAP24
  defp do_is_h264_keyframe(nalu_type, payload)
       when nalu_type in [25, 26, 27] and byte_size(payload) >= 2 do
    <<_don::16, aus::binary()>> = payload
    check_aggregation_units(nalu_type, aus)
  end

  # FU-A or FU-B
  defp do_is_h264_keyframe(nalu_type, payload)
       when nalu_type in [28..29] and byte_size(payload) >= 2 do
    <<_fu_indicator::8, fu_header::binary-size(1), _fu_payload::binary()>> = payload
    <<s::1, _e::1, _r::1, type::5>> = fu_header

    if s == 1 do
      type == 7
    else
      false
    end
  end

  defp do_is_h264_keyframe(_type, _payload), do: false

  defp check_aggregation_units(type, aus) do
    case check_first_aggregation_unit(type, aus) do
      {:error, _reason} -> false
      {false, <<>>} -> false
      {false, remaining_aus} -> check_aggregation_units(type, remaining_aus)
      {true, _remaining_aus} -> true
    end
  end

  defp check_first_aggregation_unit(type, aus) when byte_size(aus) >= 2 do
    <<nalu_size::16, rest::binary()>> = aus

    if byte_size(rest) < nalu_size do
      {:error, :au_too_short}
    else
      offset =
        case type do
          # MTAP16 - skip DOND (8 bits) and TS offset (16 bits)
          26 -> 3
          # MTAP24 - skip DOND (8 bits) and TS offset (24 bits)
          27 -> 32
          # STAP-A or STAP-B - nothing to skip
          _other -> 0
        end

      <<_x::binary-size(offset), nalu::binary-size(nalu_size), remaining_aus::binary()>> = rest
      <<0::1, _nal_ref_idc::2, nalu_type::5, _rest::binary()>> = nalu
      {nalu_type == 7, remaining_aus}
    end
  end

  defp check_first_aggregation_unit(_type, _aus), do: {:error, :au_too_short}
end
