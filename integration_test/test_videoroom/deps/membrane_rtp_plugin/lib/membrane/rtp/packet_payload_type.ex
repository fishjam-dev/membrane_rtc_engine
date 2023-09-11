defmodule Membrane.RTP.Packet.PayloadType do
  @moduledoc """
  This module contains utility to translate numerical payload type into an atom value.
  """

  alias Membrane.RTP

  @doc """
  Gets the name of used encoding from numerical payload type according to [RFC3551](https://tools.ietf.org/html/rfc3551#page-32).
  For quick reference check [datasheet](https://www.iana.org/assignments/rtp-parameters/rtp-parameters.xhtml).
  """
  @spec get_encoding_name(payload_type :: RTP.payload_type_t()) ::
          RTP.static_encoding_name_t() | :dynamic
  def get_encoding_name(type)
  def get_encoding_name(0), do: :PCMU
  def get_encoding_name(3), do: :GSM
  def get_encoding_name(4), do: :G732
  def get_encoding_name(5), do: :DVI4
  def get_encoding_name(6), do: :DVI4
  def get_encoding_name(7), do: :LPC
  def get_encoding_name(8), do: :PCMA
  def get_encoding_name(9), do: :G722
  def get_encoding_name(10), do: :L16
  def get_encoding_name(11), do: :L16
  def get_encoding_name(12), do: :QCELP
  def get_encoding_name(13), do: :CN
  def get_encoding_name(14), do: :MPA
  def get_encoding_name(15), do: :G728
  def get_encoding_name(16), do: :DVI4
  def get_encoding_name(17), do: :DVI4
  def get_encoding_name(18), do: :G729
  def get_encoding_name(25), do: :CELB
  def get_encoding_name(26), do: :JPEG
  def get_encoding_name(28), do: :NV
  def get_encoding_name(31), do: :H261
  def get_encoding_name(32), do: :MPV
  def get_encoding_name(33), do: :MP2T
  def get_encoding_name(34), do: :H263

  def get_encoding_name(payload_type) when payload_type in 96..127, do: :dynamic

  @doc """
  Gets the clock rate from numerical payload type according to [RFC3551](https://tools.ietf.org/html/rfc3551#page-32).
  For quick reference check [datasheet](https://www.iana.org/assignments/rtp-parameters/rtp-parameters.xhtml).
  """
  @spec get_clock_rate(payload_type :: RTP.payload_type_t()) ::
          RTP.clock_rate_t() | :dynamic
  def get_clock_rate(type)
  def get_clock_rate(0), do: 8000
  def get_clock_rate(3), do: 8000
  def get_clock_rate(4), do: 8000
  def get_clock_rate(5), do: 8000
  def get_clock_rate(6), do: 16_000
  def get_clock_rate(7), do: 8000
  def get_clock_rate(8), do: 8000
  def get_clock_rate(9), do: 8000
  def get_clock_rate(10), do: 44_100
  def get_clock_rate(11), do: 44_100
  def get_clock_rate(12), do: 8000
  def get_clock_rate(13), do: 8000
  def get_clock_rate(14), do: 90_000
  def get_clock_rate(15), do: 8000
  def get_clock_rate(16), do: 11_025
  def get_clock_rate(17), do: 22_050
  def get_clock_rate(18), do: 8000
  def get_clock_rate(25), do: 90_000
  def get_clock_rate(26), do: 90_000
  def get_clock_rate(28), do: 90_000
  def get_clock_rate(31), do: 90_000
  def get_clock_rate(32), do: 90_000
  def get_clock_rate(33), do: 90_000
  def get_clock_rate(34), do: 90_000

  def get_clock_rate(payload_type) when payload_type in 96..127, do: :dynamic

  @doc """
  Checks if numerical payload type should be assigned to format type dynamically.
  """
  @spec is_dynamic(payload_type :: RTP.payload_type_t()) :: boolean()
  def is_dynamic(payload_type) when payload_type in 96..127, do: true
  def is_dynamic(_payload_type), do: false
end
