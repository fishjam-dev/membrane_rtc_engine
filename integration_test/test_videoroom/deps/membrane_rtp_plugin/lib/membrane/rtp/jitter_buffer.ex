defmodule Membrane.RTP.JitterBuffer do
  @moduledoc """
  Element that buffers and reorders RTP packets based on `sequence_number`.
  """
  use Membrane.Filter
  use Bunch

  require Bitwise
  require Membrane.Logger
  alias __MODULE__.{BufferStore, Record}
  alias Membrane.RTP.Utils
  alias Membrane.{RTP, Time}

  @type packet_index :: non_neg_integer()

  @timestamp_limit Bitwise.bsl(1, 32)

  def_output_pad :output, accepted_format: RTP, demand_mode: :auto

  def_input_pad :input, accepted_format: RTP, demand_mode: :auto

  @default_latency 200 |> Time.milliseconds()

  def_options clock_rate: [spec: RTP.clock_rate_t()],
              latency: [
                spec: Time.t(),
                inspector: &Time.inspect/1,
                default: @default_latency,
                description: """
                Delay introduced by JitterBuffer
                """
              ]

  defmodule State do
    @moduledoc false
    use Bunch.Access

    defstruct store: %BufferStore{},
              clock_rate: nil,
              latency: nil,
              waiting?: true,
              max_latency_timer: nil,
              timestamp_base: nil,
              previous_timestamp: nil

    @type t :: %__MODULE__{
            store: BufferStore.t(),
            clock_rate: RTP.clock_rate_t(),
            latency: Time.t(),
            waiting?: boolean(),
            max_latency_timer: reference
          }
  end

  @impl true
  def handle_init(_ctx, %__MODULE__{latency: latency, clock_rate: clock_rate}) do
    if latency == nil do
      raise "Latancy cannot be nil"
    end

    {[], %State{latency: latency, clock_rate: clock_rate}}
  end

  @impl true
  def handle_start_of_stream(:input, _context, state) do
    Process.send_after(
      self(),
      :initial_latency_passed,
      state.latency |> Time.round_to_milliseconds()
    )

    {[], %{state | waiting?: true}}
  end

  @impl true
  def handle_end_of_stream(:input, _context, %State{store: store} = state) do
    {actions, state} =
      store
      |> BufferStore.dump()
      |> Enum.map_reduce(state, &record_to_action/2)

    {actions ++ [end_of_stream: :output], %State{state | store: %BufferStore{}}}
  end

  @impl true
  def handle_process(:input, buffer, _context, %State{store: store, waiting?: true} = state) do
    state =
      case BufferStore.insert_buffer(store, buffer) do
        {:ok, result} ->
          %State{state | store: result}

        {:error, :late_packet} ->
          Membrane.Logger.debug("Late packet has arrived")
          state
      end

    {[], state}
  end

  @impl true
  def handle_process(:input, buffer, _context, %State{store: store} = state) do
    case BufferStore.insert_buffer(store, buffer) do
      {:ok, result} ->
        state = %State{state | store: result}
        send_buffers(state)

      {:error, :late_packet} ->
        Membrane.Logger.debug("Late packet has arrived")
        {[], state}
    end
  end

  @impl true
  def handle_event(pad, event, ctx, state), do: super(pad, event, ctx, state)

  @impl true
  def handle_info(:initial_latency_passed, _context, state) do
    state = %State{state | waiting?: false}
    send_buffers(state)
  end

  @impl true
  def handle_info(:send_buffers, _context, state) do
    state = %State{state | max_latency_timer: nil}
    send_buffers(state)
  end

  defp send_buffers(%State{store: store} = state) do
    # Flushes buffers that stayed in queue longer than latency and any gaps before them
    {too_old_records, store} = BufferStore.flush_older_than(store, state.latency)
    # Additionally, flush buffers as long as there are no gaps
    {buffers, store} = BufferStore.flush_ordered(store)

    {actions, state} = (too_old_records ++ buffers) |> Enum.map_reduce(state, &record_to_action/2)

    state = %{state | store: store} |> set_timer()

    {actions, state}
  end

  @spec set_timer(State.t()) :: State.t()
  defp set_timer(%State{max_latency_timer: nil, latency: latency} = state) do
    new_timer =
      case BufferStore.first_record_timestamp(state.store) do
        nil ->
          nil

        buffer_ts ->
          since_insertion = Time.monotonic_time() - buffer_ts
          send_after_time = max(0, latency - since_insertion) |> Time.round_to_milliseconds()
          Process.send_after(self(), :send_buffers, send_after_time)
      end

    %State{state | max_latency_timer: new_timer}
  end

  defp set_timer(%State{max_latency_timer: timer} = state) when timer != nil, do: state

  defp record_to_action(nil, state) do
    action = {:event, {:output, %Membrane.Event.Discontinuity{}}}
    {action, state}
  end

  defp record_to_action(%Record{buffer: buffer}, state) do
    %{timestamp: rtp_timestamp} = buffer.metadata.rtp
    timestamp_base = state.timestamp_base || rtp_timestamp
    previous_timestamp = state.previous_timestamp || rtp_timestamp

    # timestamps in RTP don't have to be monotonic therefore there can be
    # a situation where in 2 consecutive packets the latter packet will have smaller timestamp
    # than the previous one while not overflowing the timestamp number
    # https://datatracker.ietf.org/doc/html/rfc3550#section-5.1

    timestamp_base =
      case Utils.from_which_rollover(previous_timestamp, rtp_timestamp, @timestamp_limit) do
        :next -> timestamp_base - @timestamp_limit
        :previous -> timestamp_base + @timestamp_limit
        :current -> timestamp_base
      end

    timestamp = div((rtp_timestamp - timestamp_base) * Time.second(), state.clock_rate)
    action = {:buffer, {:output, %{buffer | pts: timestamp}}}
    state = %{state | timestamp_base: timestamp_base, previous_timestamp: rtp_timestamp}
    {action, state}
  end
end
