defmodule Membrane.RTP.TWCCReceiver.PacketInfoStore do
  @moduledoc false

  # The module stores TWCC sequence number along with their arrival timestamps, handling sequence
  # number rollovers if necessary. Stored packet info can used for generating statistics used for
  # assembling a TWCC feedback packet.

  require Bitwise
  alias Membrane.RTP.Utils
  alias Membrane.Time

  defstruct base_seq_num: nil,
            max_seq_num: nil,
            seq_to_timestamp: %{}

  @type t :: %__MODULE__{
          base_seq_num: non_neg_integer(),
          max_seq_num: non_neg_integer(),
          seq_to_timestamp: %{non_neg_integer() => Time.t()}
        }

  @type stats_t :: %{
          base_seq_num: non_neg_integer(),
          packet_status_count: non_neg_integer(),
          receive_deltas: [Time.t() | :not_received],
          reference_time: Time.t()
        }

  @seq_number_limit Bitwise.bsl(1, 16)

  @spec empty?(__MODULE__.t()) :: boolean
  def empty?(%__MODULE__{base_seq_num: base_seq_num}), do: base_seq_num == nil

  @spec insert_packet_info(__MODULE__.t(), non_neg_integer()) :: __MODULE__.t()
  def insert_packet_info(store, seq_num) do
    arrival_ts = Time.vm_time()
    {store, seq_num} = maybe_handle_rollover(store, seq_num)

    %{
      store
      | base_seq_num: min(store.base_seq_num, seq_num) || seq_num,
        max_seq_num: max(store.max_seq_num, seq_num) || seq_num,
        seq_to_timestamp: Map.put(store.seq_to_timestamp, seq_num, arrival_ts)
    }
  end

  @spec get_stats(__MODULE__.t()) :: stats_t()
  def get_stats(store) do
    {reference_time, receive_deltas} = make_receive_deltas(store)
    packet_status_count = store.max_seq_num - store.base_seq_num + 1

    %{
      base_seq_num: store.base_seq_num,
      packet_status_count: packet_status_count,
      reference_time: reference_time,
      receive_deltas: receive_deltas
    }
  end

  defp maybe_handle_rollover(store, new_seq_num) do
    %{
      base_seq_num: base_seq_num,
      max_seq_num: max_seq_num,
      seq_to_timestamp: seq_to_timestamp
    } = store

    case Utils.from_which_rollover(base_seq_num, new_seq_num, @seq_number_limit) do
      :current ->
        {store, new_seq_num}

      :next ->
        {store, new_seq_num + @seq_number_limit}

      :previous ->
        shifted_seq_to_timestamp =
          Map.new(seq_to_timestamp, fn {seq_num, timestamp} ->
            {seq_num + @seq_number_limit, timestamp}
          end)

        store = %{
          store
          | base_seq_num: new_seq_num,
            max_seq_num: max_seq_num + @seq_number_limit,
            seq_to_timestamp: shifted_seq_to_timestamp
        }

        {store, new_seq_num}
    end
  end

  defp make_receive_deltas(store) do
    %{
      base_seq_num: base_seq_num,
      max_seq_num: max_seq_num,
      seq_to_timestamp: seq_to_timestamp
    } = store

    # reference time has to be in 64ms resolution
    # https://datatracker.ietf.org/doc/html/draft-holmer-rmcat-transport-wide-cc-extensions-01#section-3.1
    reference_time =
      seq_to_timestamp
      |> Map.fetch!(base_seq_num)
      |> make_divisible_by_64ms()

    receive_deltas =
      base_seq_num..max_seq_num
      |> Enum.map_reduce(reference_time, fn seq_num, previous_timestamp ->
        case Map.get(seq_to_timestamp, seq_num) do
          nil ->
            {:not_received, previous_timestamp}

          timestamp ->
            delta = timestamp - previous_timestamp
            {delta, timestamp}
        end
      end)
      |> elem(0)

    {reference_time, receive_deltas}
  end

  defp make_divisible_by_64ms(timestamp) do
    timestamp - rem(timestamp, Time.milliseconds(64))
  end
end
