defmodule Membrane.RTP.VP8.Frame do
  @moduledoc """
  Module resposible for accumulating data from RTP packets into VP8 frames
  Implements loosely algorithm described here: https://tools.ietf.org/html/rfc7741#section-4.5
  """

  alias Membrane.Buffer
  alias Membrane.RTP.VP8.Depayloader
  alias Membrane.RTP.VP8.PayloadDescriptor

  @type t :: %__MODULE__{
          fragments: [binary()],
          last_seq_num: nil | Depayloader.sequence_number(),
          last_timestamp: nil | non_neg_integer()
        }

  defstruct [:last_seq_num, :last_timestamp, fragments: []]

  defguardp is_next(last_seq_num, next_seq_num) when rem(last_seq_num + 1, 65_536) == next_seq_num
  defguardp equal_timestamp(last_timestamp, next_timestamp) when last_timestamp == next_timestamp

  @spec parse(Buffer.t(), t()) ::
          {:ok, binary(), t()}
          | {:ok, :incomplete, t()}
          | {:error,
             :packet_malformed
             | :invalid_first_packet
             | :not_rtp_buffer
             | :missing_packet
             | :timestamps_not_equal}
  def parse(rtp_buffer, acc) do
    with %Buffer{
           payload: payload,
           metadata: %{rtp: %{timestamp: timestamp, sequence_number: sequence_number}}
         } <- rtp_buffer,
         {:ok, {payload_descriptor, payload}} <-
           PayloadDescriptor.parse_payload_descriptor(payload) do
      do_parse(payload_descriptor, payload, timestamp, sequence_number, acc)
    else
      {:error, reason} -> {:error, reason}
      _not_rtp_buffer -> {:error, :not_rtp_buffer}
    end
  end

  @spec flush(__MODULE__.t()) :: {binary(), __MODULE__.t()}
  def flush(acc) do
    accumulated_frame = acc.fragments |> Enum.reverse() |> Enum.join()
    {accumulated_frame, %__MODULE__{}}
  end

  @spec do_parse(
          PayloadDescriptor.t(),
          binary(),
          non_neg_integer(),
          Depayloader.sequence_number(),
          t()
        ) ::
          {:ok, binary(), t()}
          | {:ok, :incomplete, t()}
          | {:error, :invalid_first_packet | :missing_packet | :timestamps_not_equal}
  defp do_parse(payload_descriptor, payload, timestamp, sequence_number, acc)

  # when s bit is 1 and partition_index is 0 it means that it is first packet of new frame
  defp do_parse(
         %PayloadDescriptor{s: 1, partition_index: 0},
         payload,
         timestamp,
         sequence_number,
         %__MODULE__{fragments: []} = acc
       ) do
    {:ok, :incomplete,
     %{acc | last_seq_num: sequence_number, last_timestamp: timestamp, fragments: [payload]}}
  end

  defp do_parse(
         %PayloadDescriptor{s: 1, partition_index: 0},
         payload,
         timestamp,
         sequence_number,
         acc
       ) do
    {frame, acc} = flush(acc)

    {:ok, frame,
     %{acc | last_seq_num: sequence_number, last_timestamp: timestamp, fragments: [payload]}}
  end

  # when payload descriptor indicates that it is not a first packet but accumulator is empty
  # it means that first packet is invalid
  defp do_parse(
         _payload_descriptor,
         _payload,
         _timestamp,
         _sequence_number,
         %__MODULE__{fragments: []}
       ) do
    {:error, :invalid_first_packet}
  end

  # payload is fragment of currently accumulated frame
  defp do_parse(
         _payload_descriptor,
         payload,
         timestamp,
         sequence_number,
         %__MODULE__{last_seq_num: last_seq_num, last_timestamp: last_timestamp} = acc
       )
       when is_next(last_seq_num, sequence_number) and equal_timestamp(last_timestamp, timestamp) do
    {:ok, :incomplete,
     %{acc | last_seq_num: sequence_number, fragments: [payload | acc.fragments]}}
  end

  # either timestamps are not equal or packet is missing
  defp do_parse(_payload_descriptor, _payload, timestamp, _sequence_number, %__MODULE__{
         last_timestamp: last_timestamp
       })
       when not equal_timestamp(last_timestamp, timestamp),
       do: {:error, :timestamps_not_equal}

  defp do_parse(_payload_descriptor, _payload, _timestamp, sequence_number, %__MODULE__{
         last_seq_num: last_seq_num
       })
       when not is_next(last_seq_num, sequence_number),
       do: {:error, :missing_packet}
end
