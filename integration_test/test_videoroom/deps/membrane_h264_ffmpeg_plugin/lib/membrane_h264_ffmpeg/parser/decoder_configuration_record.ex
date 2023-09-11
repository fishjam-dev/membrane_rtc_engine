defmodule Membrane.H264.FFmpeg.Parser.DecoderConfiguration do
  @moduledoc """
  Utility functions for parsing AVC Configuration Record
  """
  @enforce_keys [
    :sps,
    :pps,
    :avc_profile_indication,
    :avc_level,
    :profile_compatibility,
    :length_size
  ]
  defstruct @enforce_keys

  @type t() :: %__MODULE__{
          sps: [binary()],
          pps: [binary()],
          avc_profile_indication: non_neg_integer(),
          profile_compatibility: non_neg_integer(),
          avc_level: non_neg_integer(),
          length_size: non_neg_integer()
        }

  @spec parse(binary()) :: {:ok, t()} | {:error, any()}
  def parse(
        <<1::8, avc_profile_indication::8, profile_compatibility::8, avc_level::8, 0b111111::6,
          length_size::2, 0b111::3, rest::bitstring>>
      ) do
    {sps, rest} = parse_sps(rest)
    {pps, _rest} = parse_pps(rest)

    %__MODULE__{
      sps: sps,
      pps: pps,
      avc_profile_indication: avc_profile_indication,
      profile_compatibility: profile_compatibility,
      avc_level: avc_level,
      length_size: length_size
    }
    |> then(&{:ok, &1})
  end

  def parse(_data), do: {:error, :unknown_pattern}

  defp parse_sps(<<num_of_sps::5, rest::bitstring>>) do
    do_parse_array(num_of_sps, rest)
  end

  defp parse_pps(<<num_of_pps::8, rest::bitstring>>), do: do_parse_array(num_of_pps, rest)

  defp do_parse_array(amount, rest, acc \\ [])
  defp do_parse_array(0, rest, acc), do: {Enum.reverse(acc), rest}

  defp do_parse_array(remaining, <<size::16, data::binary-size(size), rest::bitstring>>, acc),
    do: do_parse_array(remaining - 1, rest, [data | acc])
end
