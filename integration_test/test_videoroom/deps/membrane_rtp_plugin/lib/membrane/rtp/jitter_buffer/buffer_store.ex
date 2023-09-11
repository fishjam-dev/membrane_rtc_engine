defmodule Membrane.RTP.JitterBuffer.BufferStore do
  @moduledoc false

  # Store for RTP packets. Packets are stored in `Heap` ordered by packet index. Packet index is
  # defined in RFC 3711 (SRTP) as: 2^16 * rollover count + sequence number.

  use Bunch
  use Bunch.Access
  require Bitwise
  alias Membrane.RTP.JitterBuffer.Record
  alias Membrane.RTP.{JitterBuffer, Utils}
  alias Membrane.{Buffer, RTP}

  @seq_number_limit Bitwise.bsl(1, 16)

  defstruct flush_index: nil,
            highest_incoming_index: nil,
            heap: Heap.new(&Record.rtp_comparator/2),
            set: MapSet.new(),
            rollover_count: 0

  @typedoc """
  Type describing BufferStore structure.

  Fields:
  - `rollover_count` - count of all performed rollovers (cycles of sequence number)
  - `heap` - contains records containing buffers
  - `set` - helper structure for faster read operations; content is the same as in `heap`
  - `flush_index` - index of the last packet that has been emitted (or would habe been
  emitted, but never arrived) as a result of a call to one of the `flush` functions
  - `highest_incoming_index` - the highest index in the buffer so far, mapping to the most recently produced
  RTP packet placed in JitterBuffer
  """
  @type t :: %__MODULE__{
          flush_index: JitterBuffer.packet_index() | nil,
          highest_incoming_index: JitterBuffer.packet_index() | nil,
          heap: Heap.t(),
          set: MapSet.t(),
          rollover_count: non_neg_integer()
        }

  @typedoc """
  An atom describing an error that may happen during insertion.
  """
  @type insert_error :: :late_packet

  @typedoc """
  An atom describing an error that may happen when fetching a buffer
  from the Store.
  """
  @type get_buffer_error :: :not_present

  @doc """
  Inserts buffer into the Store.

  Every subsequent buffer must have sequence number Bigger than the previously returned
  one or be part of rollover.
  """
  @spec insert_buffer(t(), Buffer.t()) :: {:ok, t()} | {:error, insert_error()}
  def insert_buffer(store, %Buffer{metadata: %{rtp: %{sequence_number: seq_num}}} = buffer) do
    do_insert_buffer(store, buffer, seq_num)
  end

  @spec do_insert_buffer(t(), Buffer.t(), RTP.Header.sequence_number_t()) ::
          {:ok, t()} | {:error, insert_error()}
  defp do_insert_buffer(%__MODULE__{flush_index: nil} = store, buffer, 0) do
    store = add_record(store, Record.new(buffer, @seq_number_limit), :next)
    {:ok, %__MODULE__{store | flush_index: @seq_number_limit - 1}}
  end

  defp do_insert_buffer(%__MODULE__{flush_index: nil} = store, buffer, seq_num) do
    store = add_record(store, Record.new(buffer, seq_num), :current)
    {:ok, %__MODULE__{store | flush_index: seq_num - 1}}
  end

  defp do_insert_buffer(
         %__MODULE__{
           flush_index: flush_index,
           highest_incoming_index: highest_incoming_index,
           rollover_count: roc
         } = store,
         buffer,
         seq_num
       ) do
    highest_seq_num = rem(highest_incoming_index, @seq_number_limit)

    {rollover, index} =
      case Utils.from_which_rollover(highest_seq_num, seq_num, @seq_number_limit) do
        :current -> {:current, seq_num + roc * @seq_number_limit}
        :previous -> {:previous, seq_num + (roc - 1) * @seq_number_limit}
        :next -> {:next, seq_num + (roc + 1) * @seq_number_limit}
      end

    if is_fresh_packet?(flush_index, index) do
      record = Record.new(buffer, index)
      {:ok, add_record(store, record, rollover)}
    else
      {:error, :late_packet}
    end
  end

  @doc """
  Flushes the store to the buffer with the next sequence number.

  If this buffer is present, it will be returned.
  Otherwise it will be treated as late and rejected on attempt to insert into the store.
  """
  @spec flush_one(t) :: {Record.t() | nil, t}
  def flush_one(store)

  def flush_one(%__MODULE__{flush_index: nil} = store) do
    {nil, store}
  end

  def flush_one(%__MODULE__{flush_index: flush_index, heap: heap, set: set} = store) do
    record = Heap.root(heap)

    expected_next_index = flush_index + 1

    {result, store} =
      if record != nil and record.index == expected_next_index do
        updated_heap = Heap.pop(heap)
        updated_set = MapSet.delete(set, record.index)

        updated_store = %__MODULE__{store | heap: updated_heap, set: updated_set}

        {record, updated_store}
      else
        # TODO: instead of nil use expected_next_index to put in Discontinuity metadata
        #       after https://github.com/membraneframework/membrane-core/issues/238 is done.
        {nil, store}
      end

    {result, %__MODULE__{store | flush_index: expected_next_index}}
  end

  @doc """
  Flushes the store until the first gap in sequence numbers of records
  """
  @spec flush_ordered(t) :: {[Record.t() | nil], t}
  def flush_ordered(store) do
    flush_while(store, fn %__MODULE__{flush_index: flush_index}, %Record{index: index} ->
      index == flush_index + 1
    end)
  end

  @doc """
  Flushes the store as long as it contains a buffer with the timestamp older than provided duration
  """
  @spec flush_older_than(t, Membrane.Time.t()) :: {[Record.t() | nil], t}
  def flush_older_than(store, max_age) do
    max_age_timestamp = Membrane.Time.monotonic_time() - max_age

    flush_while(store, fn _store, %Record{timestamp: timestamp} ->
      timestamp <= max_age_timestamp
    end)
  end

  @doc """
  Returns all buffers that are stored in the `BufferStore`.
  """
  @spec dump(t()) :: [Record.t()]
  def dump(%__MODULE__{} = store) do
    {records, _store} = flush_while(store, fn _store, _record -> true end)
    records
  end

  @doc """
  Returns timestamp (time of insertion) of a buffer with lowest index
  """
  @spec first_record_timestamp(t()) :: Membrane.Time.t() | nil
  def first_record_timestamp(%__MODULE__{heap: heap}) do
    case Heap.root(heap) do
      %Record{timestamp: time} -> time
      nil -> nil
    end
  end

  defp is_fresh_packet?(flush_index, index), do: index > flush_index

  @spec flush_while(t, (t, Record.t() -> boolean), [Record.t() | nil]) ::
          {[Record.t() | nil], t}
  defp flush_while(%__MODULE__{heap: heap} = store, fun, acc \\ []) do
    heap
    |> Heap.root()
    |> case do
      nil ->
        {Enum.reverse(acc), store}

      record ->
        if fun.(store, record) do
          {record, store} = flush_one(store)
          flush_while(store, fun, [record | acc])
        else
          {Enum.reverse(acc), store}
        end
    end
  end

  defp add_record(%__MODULE__{heap: heap, set: set} = store, %Record{} = record, record_rollover) do
    if set |> MapSet.member?(record.index) do
      store
    else
      %__MODULE__{store | heap: Heap.push(heap, record), set: MapSet.put(set, record.index)}
      |> update_highest_incoming_index(record.index)
      |> update_roc(record_rollover)
    end
  end

  defp update_highest_incoming_index(
         %__MODULE__{highest_incoming_index: last} = store,
         added_index
       )
       when added_index > last or last == nil,
       do: %__MODULE__{store | highest_incoming_index: added_index}

  defp update_highest_incoming_index(
         %__MODULE__{highest_incoming_index: last} = store,
         added_index
       )
       when last >= added_index,
       do: store

  defp update_roc(%{rollover_count: roc} = store, :next),
    do: %__MODULE__{store | rollover_count: roc + 1}

  defp update_roc(store, _record_rollover), do: store
end
