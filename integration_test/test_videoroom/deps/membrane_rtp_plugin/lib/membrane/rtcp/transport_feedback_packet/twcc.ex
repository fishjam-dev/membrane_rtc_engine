defmodule Membrane.RTCP.TransportFeedbackPacket.TWCC do
  @moduledoc """
  Encodes and decodes [Transport-wide congestion control](https://datatracker.ietf.org/doc/html/draft-holmer-rmcat-transport-wide-cc-extensions-01)
  feedback packets.
  """
  @behaviour Membrane.RTCP.TransportFeedbackPacket

  require Bitwise
  require Membrane.Logger
  alias Membrane.Time

  defmodule RunLength do
    @moduledoc false
    defstruct [:packet_status, :packet_count]
  end

  defmodule StatusVector do
    @moduledoc false
    defstruct [:vector, :packet_count]
  end

  defstruct [
    :base_seq_num,
    :reference_time,
    :packet_status_count,
    :receive_deltas,
    :feedback_packet_count
  ]

  @type t :: %__MODULE__{
          base_seq_num: non_neg_integer(),
          reference_time: Time.t(),
          packet_status_count: pos_integer(),
          receive_deltas: [Time.t() | :not_received],
          feedback_packet_count: non_neg_integer()
        }

  @max_u8_val Bitwise.bsl(1, 8) - 1
  @max_u13_val Bitwise.bsl(1, 13) - 1
  @max_s16_val Bitwise.bsl(1, 15) - 1
  @min_s16_val Bitwise.bsl(-1, 15)

  @small_delta_range 0..@max_u8_val

  @run_length_id 0
  @run_length_capacity @max_u13_val

  @status_vector_id 1
  @status_vector_symbol_1_bit_id 0
  @status_vector_symbol_2_bit_id 1
  @status_vector_capacity 7

  @packet_status_flags BiMap.new(%{
                         not_received: 0,
                         small_delta: 1,
                         large_or_negative_delta: 2,
                         reserved: 3
                       })

  @impl true
  def decode(
        <<base_seq_num::16, packet_status_count::16, reference_time::signed-integer-size(24),
          feedback_packet_count::8, payload::binary>> = packet
      ) do
    case parse_feedback(payload, packet_status_count) do
      {:ok, receive_deltas} ->
        {:ok,
         %__MODULE__{
           base_seq_num: base_seq_num,
           reference_time: Time.milliseconds(reference_time) * 64,
           packet_status_count: packet_status_count,
           receive_deltas: receive_deltas,
           feedback_packet_count: feedback_packet_count
         }}

      {:error, reason} ->
        Membrane.Logger.debug("""
        An error occured while parsing TWCC feedback packet.
        Reason: #{reason}
        Packet: #{inspect(packet, limit: :infinity)}
        """)

        {:error, :malformed_packet}
    end
  end

  @impl true
  def encode(%__MODULE__{} = stats) do
    %{
      base_seq_num: base_seq_num,
      reference_time: reference_time,
      packet_status_count: packet_status_count,
      receive_deltas: receive_deltas,
      feedback_packet_count: feedback_packet_count
    } = stats

    scaled_receive_deltas = Enum.map(receive_deltas, &scale_delta/1)
    packet_status_chunks = make_packet_status_chunks(scaled_receive_deltas)

    # reference time has to be in 64ms resolution
    # https://datatracker.ietf.org/doc/html/draft-holmer-rmcat-transport-wide-cc-extensions-01#section-3.1
    reference_time = div(reference_time, Time.milliseconds(64))

    encoded_header =
      <<base_seq_num::16, packet_status_count::16, reference_time::signed-integer-size(24),
        feedback_packet_count::8>>

    encoded_packet_status_chunks = Enum.map(packet_status_chunks, &encode_packet_status_chunk/1)

    encoded_receive_deltas = Enum.map(scaled_receive_deltas, &encode_receive_delta/1)

    [encoded_header, encoded_packet_status_chunks, encoded_receive_deltas]
    |> IO.iodata_to_binary()
    |> maybe_add_padding()
  end

  defp parse_feedback(payload, packet_status_count) do
    with {:ok, {encoded_receive_deltas, parsed_packet_status}} <-
           parse_packet_status(payload, packet_status_count, []) do
      parse_receive_deltas(encoded_receive_deltas, parsed_packet_status, [])
    end
  end

  defp parse_packet_status(rest, packets_left, parsed_status) when packets_left <= 0 do
    # note about incomplete vectors: the draft does not specify this, but libwebrtc can make the last
    # status vector incomplete, filling the untaken slots with 0s - we may need to drop them
    parsed_status = Enum.drop(parsed_status, packets_left)

    {:ok, {rest, parsed_status}}
  end

  defp parse_packet_status(
         <<@run_length_id::1, packet_status::2, run_length::unsigned-integer-size(13),
           rest::binary>>,
         packets_left,
         parsed_status
       )
       when run_length <= packets_left do
    new_status =
      @packet_status_flags
      |> BiMap.fetch_key!(packet_status)
      |> List.duplicate(run_length)

    parse_packet_status(rest, packets_left - run_length, parsed_status ++ new_status)
  end

  defp parse_packet_status(
         <<@status_vector_id::1, vector_type::1, symbol_list::bits-size(14), rest::binary>>,
         packets_left,
         parsed_status
       ) do
    {symbol_size, packets_parsed} =
      case vector_type do
        @status_vector_symbol_1_bit_id -> {1, 14}
        @status_vector_symbol_2_bit_id -> {2, 7}
      end

    # note about 1-bit symbols: the draft does not specify this,
    # but libwebrtc treats <<1::1>> as a "packet received, small delta" status
    new_status =
      for <<(<<symbol::size(symbol_size)>> <- symbol_list)>>,
        do: BiMap.fetch_key!(@packet_status_flags, symbol)

    parse_packet_status(rest, packets_left - packets_parsed, parsed_status ++ new_status)
  end

  defp parse_packet_status(_payload, _packets_left, _parsed_status),
    do: {:error, :invalid_run_length}

  defp parse_receive_deltas(padding, [], parsed_deltas) do
    if :binary.decode_unsigned(padding) != 0 do
      {:error, :invalid_padding}
    else
      {:ok, Enum.reverse(parsed_deltas)}
    end
  end

  defp parse_receive_deltas(rest, [:not_received | rest_status], parsed_deltas) do
    parse_receive_deltas(rest, rest_status, [:not_received | parsed_deltas])
  end

  defp parse_receive_deltas(
         <<delta::unsigned-integer-size(8), rest::binary>>,
         [:small_delta | rest_status],
         parsed_deltas
       ) do
    parse_receive_deltas(rest, rest_status, [Time.microseconds(delta * 250) | parsed_deltas])
  end

  defp parse_receive_deltas(
         <<delta::signed-integer-size(16), rest::binary>>,
         [:large_or_negative_delta | rest_status],
         parsed_deltas
       ) do
    parse_receive_deltas(rest, rest_status, [Time.microseconds(delta * 250) | parsed_deltas])
  end

  defp parse_receive_deltas(_payload, [:reserved | _rest_status], _parsed_deltas),
    do: {:error, :symbol_reserved}

  defp make_packet_status_chunks(scaled_receive_deltas) do
    scaled_receive_deltas
    |> Enum.map(&delta_to_packet_status/1)
    |> Enum.reverse()
    |> Enum.reduce([], fn status, acc ->
      case acc do
        [%RunLength{packet_count: @run_length_capacity} | _rest] ->
          [%RunLength{packet_status: status, packet_count: 1} | acc]

        [%RunLength{packet_status: ^status, packet_count: count} = run_length | rest] ->
          [%{run_length | packet_count: count + 1} | rest]

        # Got a different packet status and the current chunk is of run length type.
        # If the condition is fulfilled, it's viable to convert it to a status vector.
        [%RunLength{packet_count: count} = run_length | rest] when count < @status_vector_capacity ->
          %StatusVector{vector: vector} = run_length_to_status_vector(run_length)
          [%StatusVector{vector: [status | vector], packet_count: count + 1} | rest]

        # TODO: if no large or negative delta has been encountered, consider using 1-bit status vector

        [%StatusVector{vector: vector, packet_count: count} | rest]
        when count < @status_vector_capacity ->
          [%StatusVector{vector: [status | vector], packet_count: count + 1} | rest]

        _acc_empty_or_status_vector_full_or_conversion_not_viable ->
          [%RunLength{packet_status: status, packet_count: 1} | acc]
      end
    end)
  end

  defp encode_receive_delta(scaled_delta) do
    case delta_to_packet_status(scaled_delta) do
      :small_delta ->
        <<scaled_delta::8>>

      :large_or_negative_delta ->
        Membrane.Logger.debug(
          "Reporting a packet with large or negative delta: (#{inspect(scaled_delta / 4)}ms)"
        )

        <<cap_delta(scaled_delta)::16>>

      :not_received ->
        Membrane.Logger.debug("Reporting a non-received packet")
        <<>>
    end
  end

  defp encode_packet_status_chunk(%RunLength{packet_status: status, packet_count: packet_count}),
    do: <<@run_length_id::1, BiMap.fetch!(@packet_status_flags, status)::2, packet_count::13>>

  defp encode_packet_status_chunk(%StatusVector{vector: vector, packet_count: packet_count}) do
    symbol_list =
      Enum.reduce(vector, <<>>, fn status, acc ->
        <<acc::bitstring, BiMap.fetch!(@packet_status_flags, status)::2>>
      end)

    # in current implementation we use only 2-bit symbols for encoding, so padding
    # size for an incomplete vector is always 2*(number of unfilled slots) bits
    padding_size = 2 * (@status_vector_capacity - packet_count)

    <<@status_vector_id::1, @status_vector_symbol_2_bit_id::1, symbol_list::bitstring,
      0::size(padding_size)>>
  end

  defp run_length_to_status_vector(%RunLength{packet_status: status, packet_count: count}),
    do: %StatusVector{vector: List.duplicate(status, count), packet_count: count}

  defp scale_delta(:not_received), do: :not_received

  defp scale_delta(delta) do
    # Deltas are represented as multiples of 250μs
    # https://datatracker.ietf.org/doc/html/draft-holmer-rmcat-transport-wide-cc-extensions-01#section-3.1.5
    delta |> Time.round_to_microseconds() |> div(250)
  end

  defp delta_to_packet_status(scaled_delta) do
    cond do
      scaled_delta == :not_received -> :not_received
      scaled_delta in @small_delta_range -> :small_delta
      true -> :large_or_negative_delta
    end
  end

  defp cap_delta(scaled_delta) do
    cond do
      scaled_delta < @min_s16_val -> @min_s16_val
      scaled_delta > @max_s16_val -> @max_s16_val
      true -> scaled_delta
    end
  end

  defp maybe_add_padding(payload) do
    bytes_remaining = rem(byte_size(payload), 4)

    if bytes_remaining > 0 do
      padding_size = 4 - bytes_remaining
      <<payload::binary, 0::size(padding_size)-unit(8)>>
    else
      payload
    end
  end
end
