defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocator do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator
  use GenServer
  use Bunch.Access

  require Logger

  alias __MODULE__.AllocationGrantedNotification

  alias Membrane.{Buffer, Time}

  @typep track_receiver_metadata() :: %{
           pid: pid(),
           current_allocation: number(),
           target_allocation: number() | nil,
           negotiable?: boolean()
         }

  @opaque state_t() :: %__MODULE__{
            track_receivers: %{pid() => track_receiver_metadata()},
            probing_queue: Qex.t(),
            available_bandwidth: non_neg_integer() | :unknown,
            probing_target_bitrate: non_neg_integer(),
            allocated_bandwidth: non_neg_integer(),
            bitrate_timer: :timer.tref() | nil,
            estimation_timestamp: integer(),
            bits_sent: non_neg_integer()
          }

  defstruct [
    :bitrate_timer,
    probing_target_bitrate: 0,
    available_bandwidth: :unknown,
    track_receivers: %{},
    allocated_bandwidth: 0,
    estimation_timestamp: 0,
    bits_sent: 0,
    probing_queue: Qex.new()
  ]

  @padding_packet_size 8 * Membrane.RTP.Packet.padding_packet_size()

  @spec start_link() :: GenServer.on_start()
  def start_link(), do: GenServer.start_link(__MODULE__, [], [])

  ## Public API

  @impl true
  def say_hello(prober, bandwidth, track),
    do: GenServer.cast(prober, {:hello, self(), bandwidth, track})

  @impl true
  def request_allocation(bitrate_manager, desired_allocation),
    do: GenServer.cast(bitrate_manager, {:request_allocation, self(), desired_allocation})

  @impl true
  def update_bandwidth_estimation(prober, estimation),
    do: GenServer.cast(prober, {:bandwidth_estimation, estimation})

  @impl true
  def buffer_sent(prober, %Buffer{payload: payload}),
    do: GenServer.cast(prober, {:bits_sent, bit_size(payload)})

  @impl true
  def probe_sent(prober),
    do: GenServer.cast(prober, {:bits_sent, @padding_packet_size})

  @impl true
  @spec init(any) ::
          {:ok,
           %{
             bandwidth_estimation: nil,
             bitrate_timer: nil,
             bytes_sent: 0,
             estimation_timestamp: 0,
             track_receivers: Qex.t()
           }}
  def init(_opts) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_cast({:bandwidth_estimation, estimation}, state) do
    Logger.info("Received bandwidth estimation of #{estimation / 1024} kbps")

    state =
      state
      |> Map.put(:available_bandwidth, estimation)
      |> Map.put(:probing_target_bitrate, estimation + 200)
      |> update_allocations()
      |> stop_timer()
      |> maybe_start_timer()

    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:bits_sent, size}, state) do
    state = Map.update!(state, :bits_sent, &(&1 + size))
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:hello, pid, bandwidth, track}, state) do
    # This is the very first call that we're getting from the Track Receiver
    # It is already sending some variant, so whatever bandwidth they are using will be initially allocated
    # without question

    receiver = %{
      pid: pid,
      current_allocation: bandwidth,
      target_allocation: nil,
      negotiable?: track.type == :video
    }

    state =
      if track.type == :video do
        Map.update!(state, :probing_queue, &Qex.push(&1, pid))
      else
        state
      end

    state =
      state
      |> put_in([:track_receivers, pid], receiver)
      |> Map.update!(:allocated_bandwidth, &(&1 + bandwidth))
      |> update_allocations()

    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:request_allocation, pid, target}, state) do
    Logger.info("Receiver #{inspect(pid)} requested allocation of #{target / 1024} kbps")
    receiver = Map.fetch!(state.track_receivers, pid)

    state =
      if receiver.current_allocation >= target or not receiver.negotiable? do
        # Receiver lowers its allocation or this TR has non-negotiable allocation. It is therefore always granted

        send(pid, %AllocationGrantedNotification{allocation: target})

        state
        |> put_in([:track_receivers, pid], %{
          receiver
          | current_allocation: target,
            target_allocation: nil
        })
        |> Map.update!(:allocated_bandwidth, &(&1 - receiver.current_allocation + target))
        |> update_allocations()
      else
        # Receiver raises its allocation. This might not be instantly granted
        state
        |> put_in([:track_receivers, pid], %{receiver | target_allocation: target})
        |> update_allocations()
      end

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:check_bits_sent, state) do
    use Numbers, overload_operators: true

    now = get_timestamp()
    elapsed_time_in_s = Time.as_seconds(now - state.estimation_timestamp)
    expected_bits = elapsed_time_in_s * state.probing_target_bitrate
    missing = expected_bits - state.bits_sent

    state =
      if Ratio.to_float(missing) > 0 do
        # Send paddings

        no_padding_packets =
          missing
          |> Ratio.new(@padding_packet_size)
          |> Ratio.ceil()

        send_padding_packets(state, no_padding_packets)
      else
        state
      end

    {:noreply, state}
  end

  ## Helper functions
  defp stop_timer(%__MODULE__{bitrate_timer: nil} = state), do: state

  defp stop_timer(%__MODULE__{} = state) do
    {:ok, :cancel} = :timer.cancel(state.bitrate_timer)
    %{state | bitrate_timer: nil}
  end

  defp maybe_stop_timer(%__MODULE__{} = state) do
    if is_deficient?(state), do: state, else: stop_timer(state)
  end

  defp maybe_start_timer(%__MODULE__{} = state) do
    # we're not probing if we estimate infinite bandwidth or when none of the tracks is deficient
    if state.available_bandwidth != :unknown and is_deficient?(state) do
      {:ok, timer} = :timer.send_interval(10, :check_bits_sent)
      %{state | bitrate_timer: timer, estimation_timestamp: get_timestamp(), bits_sent: 0}
    else
      state
    end
  end

  defp is_deficient?(state),
    do:
      state.track_receivers
      |> Map.values()
      |> Enum.any?(&(not is_nil(&1.target_allocation)))

  defp get_timestamp(), do: System.monotonic_time(:nanosecond)

  defp send_padding_packets(state, 0), do: state

  defp send_padding_packets(state, packets_num) do
    Enum.reduce(1..packets_num, state, fn _i, state ->
      # We need to select a track receiver in such a way that each one sends an equal amount of packets to create
      # => Round Robin
      {tr, queue} = Qex.pop!(state.probing_queue)
      send(tr, :send_padding_packet)

      %{state | probing_queue: Qex.push(queue, tr)}
    end)
  end

  defp update_allocations(%__MODULE__{available_bandwidth: :unknown} = state), do: state

  defp update_allocations(%__MODULE__{track_receivers: track_receivers} = state)
       when state.allocated_bandwidth > state.available_bandwidth do
    negotiable_trs = track_receivers |> Map.values() |> Enum.count(& &1.negotiable?)

    if negotiable_trs > 0 do
      non_negotiable_bandwidth =
        track_receivers
        |> Map.values()
        |> Enum.filter(&(not &1.negotiable?))
        |> Enum.map(& &1.current_allocation)
        |> Enum.sum()

      allocation = (state.available_bandwidth - non_negotiable_bandwidth) / negotiable_trs

      track_receivers =
        Map.new(track_receivers, fn
          {k, %{negotiable?: false} = v} -> {k, v}
          {k, v} -> {k, %{v | current_allocation: allocation, target_allocation: nil}}
        end)

      for receiver <- Map.values(track_receivers) do
        send(receiver.pid, %AllocationGrantedNotification{allocation: receiver.current_allocation})
      end

      %{state | track_receivers: track_receivers, allocated_bandwidth: state.available_bandwidth}
    else
      Logger.warn("We're using more bandwidth then we have, but we cannot lower our usage")
      state
    end
  end

  defp update_allocations(%__MODULE__{available_bandwidth: bandwidth} = state) do
    free_bandwidth = bandwidth - state.allocated_bandwidth

    state.track_receivers
    |> Map.values()
    |> Enum.reject(&is_nil(&1.target_allocation))
    |> Enum.find(&(&1.target_allocation - &1.current_allocation <= free_bandwidth))
    |> case do
      nil ->
        maybe_stop_timer(state)

      receiver ->
        pid = receiver.pid
        send(pid, %AllocationGrantedNotification{allocation: receiver.target_allocation})

        state
        |> update_in(
          [:track_receivers, pid],
          &%{&1 | target_allocation: nil, current_allocation: &1.target_allocation}
        )
        |> Map.update!(
          :allocated_bandwidth,
          &(&1 + receiver.target_allocation - receiver.current_allocation)
        )
        |> update_allocations()
    end
  end
end
