defmodule Membrane.RTP.H264.Utils do
  @moduledoc """
  Utility functions for RTP packets containing H264 encoded frames.
  """

  @doc """
  Checks whether RTP payload contains H264 keyframe.

  By default, with option `look_for` set to `:sps`, will in some cases check
  whether the payload contains SPS (NALU payload type 7);
  if `look_for` is set to `:idr`, will look exclusively for IDR frames
  (NALU payload type 5).
  """
  # This is rewritten from galene
  # https://github.com/jech/galene/blob/6fbdf0eab2c9640e673d9f9ec0331da24cbf2c4c/codecs/codecs.go#L119
  # and based on https://datatracker.ietf.org/doc/html/rfc6184#section-5
  #
  # it is also unclear why we sometimes check against nalu type == 7
  # and sometimes against nalu type == 5 but galene does it this way
  # and it works
  @spec is_keyframe(binary(), :sps | :idr) :: boolean()
  def is_keyframe(rtp_payload, look_for \\ :sps)

  def is_keyframe(rtp_payload, _look_for) when byte_size(rtp_payload) < 1, do: false

  def is_keyframe(rtp_payload, look_for) do
    <<_f::1, _nri::2, nalu_type::5, rest::binary>> = rtp_payload
    do_is_keyframe(nalu_type, rest, look_for)
  end

  # reserved
  defp do_is_keyframe(0, _payload, _look_for), do: false

  # single NALU
  defp do_is_keyframe(nalu_type, _nalu, _look_for) when nalu_type in 1..23 do
    nalu_type == 5
  end

  # STAP-A
  defp do_is_keyframe(24, aus, look_for) do
    # aus - aggregation units
    # those might be single-time or multi-time aggreagtion units
    check_aggregation_units(24, aus, look_for)
  end

  # STAP-B, MTAP16 or MTAP24
  defp do_is_keyframe(nalu_type, payload, look_for)
       when nalu_type in 25..27 and byte_size(payload) >= 2 do
    <<_don::16, aus::binary>> = payload
    check_aggregation_units(nalu_type, aus, look_for)
  end

  # FU-A or FU-B
  defp do_is_keyframe(nalu_type, payload, look_for)
       when nalu_type in 28..29 and byte_size(payload) >= 1 do
    # FU indicator has already been cut off
    <<fu_header::binary-size(1), _fu_payload::binary>> = payload
    <<s::1, _e::1, _r::1, type::5>> = fu_header

    case look_for do
      :sps ->
        s == 1 and type == 7

      :idr ->
        s == 1 and type == 5
    end
  end

  defp do_is_keyframe(_type, _payload, _look_for), do: false

  defp check_aggregation_units(type, aus, look_for) do
    case check_first_aggregation_unit(type, aus, look_for) do
      {:error, _reason} -> false
      {false, <<>>} -> false
      {false, remaining_aus} -> check_aggregation_units(type, remaining_aus, look_for)
      {true, _remaining_aus} -> true
    end
  end

  defp check_first_aggregation_unit(type, aus, look_for) when byte_size(aus) >= 2 do
    <<nalu_size::16, rest::binary>> = aus

    if byte_size(rest) < nalu_size do
      {:error, :au_too_short}
    else
      offset =
        case type do
          # MTAP16 - skip DOND (8 bits) and TS offset (16 bits)
          26 -> 3
          # MTAP24 - skip DOND (8 bits) and TS offset (24 bits)
          27 -> 4
          # STAP-A or STAP-B - nothing to skip
          _other -> 0
        end

      <<_x::binary-size(offset), nalu::binary-size(nalu_size), remaining_aus::binary>> = rest
      <<0::1, _nal_ref_idc::2, nalu_type::5, _rest::binary>> = nalu

      case look_for do
        :sps ->
          {nalu_type == 7, remaining_aus}

        :idr ->
          {nalu_type == 5, remaining_aus}
      end
    end
  end

  defp check_first_aggregation_unit(_type, _aus, _look_for), do: {:error, :au_too_short}
end
