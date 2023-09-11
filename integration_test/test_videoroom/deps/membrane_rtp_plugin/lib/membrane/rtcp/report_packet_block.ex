defmodule Membrane.RTCP.ReportPacketBlock do
  @moduledoc """
  Parses and constructs report blocks, which are parts of RTCP Sender and Receiver Reports
  defined in [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.4)
  """

  @enforce_keys [
    :ssrc,
    :fraction_lost,
    :total_lost,
    :highest_seq_num,
    :interarrival_jitter,
    :last_sr_timestamp,
    :delay_since_sr
  ]

  defstruct @enforce_keys

  @type t() :: %__MODULE__{
          ssrc: non_neg_integer(),
          fraction_lost: float(),
          total_lost: non_neg_integer(),
          highest_seq_num: non_neg_integer(),
          interarrival_jitter: non_neg_integer(),
          last_sr_timestamp: non_neg_integer(),
          delay_since_sr: non_neg_integer()
        }

  @spec encode(t()) :: binary()
  def encode(block) do
    %{
      ssrc: ssrc,
      fraction_lost: fraction_lost,
      total_lost: total_lost,
      highest_seq_num: max_seq_num,
      interarrival_jitter: jitter,
      last_sr_timestamp: last_sr_timestamp,
      delay_since_sr: delay_since_sr
    } = block

    fixed_point_fraction = round(fraction_lost * 256)

    <<ssrc::32, fixed_point_fraction::8, total_lost::24, max_seq_num::32, jitter::32,
      last_sr_timestamp::32, delay_since_sr::32>>
  end

  @spec decode(binary()) :: {:ok, [t()]} | {:error, :invalid_report_block}
  def decode(blocks), do: decode(blocks, [])

  defp decode(<<>>, acc), do: {:ok, acc}

  defp decode(
         <<ssrc::32, fraction_lost::8, total_lost::24, max_seq_num::32, jitter::32,
           last_sr_timestamp::32, delay_since_sr::32, rest::binary>>,
         acc
       ) do
    data = %__MODULE__{
      ssrc: ssrc,
      fraction_lost: fraction_lost / 256,
      total_lost: total_lost,
      highest_seq_num: max_seq_num,
      interarrival_jitter: jitter,
      last_sr_timestamp: last_sr_timestamp,
      delay_since_sr: delay_since_sr
    }

    decode(rest, [data | acc])
  end

  defp decode(_blocks, _acc), do: {:error, :invalid_report_block}
end
