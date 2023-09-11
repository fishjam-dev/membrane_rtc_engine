defmodule Membrane.RTP do
  @moduledoc """
  This module provides caps struct for RTP packet.
  """

  @typedoc """
  RTP payload type that is statically mapped to encoding and clock rate.
  """
  @type static_payload_type_t :: 0..95

  @typedoc """
  RTP payload type that can be dynamically mapped to encoding and clock rate.
  """
  @type dynamic_payload_type_t :: 96..127

  @typedoc """
  RTP payload type.
  """
  @type payload_type_t() :: static_payload_type_t() | dynamic_payload_type_t()

  @typedoc """
  Predefined names of encoding for static payload types (< 96).
  """
  @type static_encoding_name_t() ::
          :PCMU
          | :GSM
          | :G732
          | :DVI4
          | :LPC
          | :PCMA
          | :G722
          | :L16
          | :QCELP
          | :CN
          | :MPA
          | :G728
          | :G729
          | :CELB
          | :JPEG
          | :NV
          | :H261
          | :MPV
          | :MP2T
          | :H263

  @typedoc """
  Dynamically assigned encoding names for payload types >= 96

  They should be atoms matching the encoding name in SDP's attribute `rtpmap`. This is usually defined by
  RFC for that payload format (e.g. for H264 there's RFC 6184 defining it must be "H264", so the atom `:H264` should be used
  https://tools.ietf.org/html/rfc6184#section-8.2.1)
  """
  @type dynamic_encoding_name_t() :: atom()

  @typedoc """
  Encoding name of RTP payload.
  """
  @type encoding_name_t() :: static_encoding_name_t | dynamic_encoding_name_t()

  @typedoc """
  Rate of the clock used for RTP timestamps in Hz
  """
  @type clock_rate_t() :: non_neg_integer()

  @typedoc """
  The source of a stream of RTP packets, identified by a 32-bit numeric identifier.
  """
  @type ssrc_t() :: pos_integer()

  @type t() :: %__MODULE__{}

  defstruct []

  @doc """
  Determines if payload type is `t:static_payload_type_t/0`.
  """
  defguard is_payload_type_static(payload_type) when payload_type in 0..95

  @doc """
  Determines if payload type is `t:dynamic_payload_type_t/0`.
  """
  defguard is_payload_type_dynamic(payload_type) when payload_type in 96..127
end
