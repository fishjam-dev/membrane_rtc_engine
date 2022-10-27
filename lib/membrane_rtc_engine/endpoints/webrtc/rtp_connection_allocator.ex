defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocator do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator

  use GenServer
  use Bunch.Access

  require Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.AllocationGrantedNotification
  alias Membrane.{Buffer, Time}

  @typep track_receiver_metadata() :: %{
           pid: pid(),
           current_allocation: number(),
           target_allocation: number() | nil,
           negotiable?: boolean()
         }

  # Type describing a status of RTPConnectionAllocator's prober part.

  # - `:increase_estimation` - actively probing slightly over current estimation in order to increase it
  # - `:maintain_estmation` - only sending enough probes to make sure that the estimation doesn't fall
  #   below the level of allocated bandwidth
  # - `:allowed_bandwidth_deficiency` - we have allocated more bandwidth than the estimation, but the estimation
  #  is increasing. It's only possible to reach this state though allocations from non-negotiable Track
  #  Receivers and through new Track Receiver registering
  # - `:disallowed_bandwidth_deficiency` - we have allocated more bandwidth than estimated. This state can be reached
  #  when we're in `:allowed_bandwidth_deficiency` and estimation decreased, or if the estimation decreases below
  #  the level of total allocation
  @typep prober_status_t() ::
           :increase_estimation
           | :maintain_estimation
           | :allowed_bandwidth_deficiency
           | :disallowed_bandwidth_deficiency

  @opaque t() :: %__MODULE__{
            track_receivers: %{pid() => track_receiver_metadata()},
            probing_queue: Qex.t(),
            prober_status: prober_status_t(),
            available_bandwidth: non_neg_integer() | :unknown,
            allocated_bandwidth: non_neg_integer(),
            bitrate_timer: :timer.tref() | nil,
            estimation_timestamp: integer(),
            bits_sent: non_neg_integer()
          }

  defstruct [
    :bitrate_timer,
    available_bandwidth: :unknown,
    prober_status: :increase_estimation,
    track_receivers: %{},
    allocated_bandwidth: 0,
    estimation_timestamp: 0,
    bits_sent: 0,
    probing_queue: Qex.new()
  ]

  @padding_packet_size 8 * 256

  @impl true
  def start_link(), do: GenServer.start_link(__MODULE__, [], [])

  ## Public API

  @impl true
  def register_track_receiver(prober, bandwidth, track),
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
  def init(_opts) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_cast({:bandwidth_estimation, estimation}, state) do
    Logger.info("Received bandwidth estimation of #{estimation / 1024} kbps")
    estimation_increasing? = estimation >= state.available_bandwidth

    state =
      state
      |> Map.put(:available_bandwidth, estimation)
      |> update_deficiency_statuses(estimation_increasing?)
      |> update_allocations()
      |> update_probing_statuses()
      |> stop_probing_timer()
      |> start_probing_timer()

    {:noreply, state}
  end

  @impl true
  def handle_cast({:bits_sent, size}, state) do
    state = Map.update!(state, :bits_sent, &(&1 + size))
    {:noreply, state}
  end

  @impl true
  def handle_cast({:hello, pid, bandwidth, track}, state) do
    # This is the very first call that we're getting from the Track Receiver
    # It is already sending some variant, so whatever bandwidth they are using will be initially allocated
    # without question

    receiver = %{
      pid: pid,
      current_allocation: bandwidth,
      target_allocation: nil,
      negotiable?: length(track.variants) > 1
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
      |> update_prober_status(deficiency_allowed?: true)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:request_allocation, pid, target}, state) do
    Logger.info("Receiver #{inspect(pid)} requested allocation of #{target / 1024} kbps")
    receiver = Map.fetch!(state.track_receivers, pid)

    state =
      cond do
        receiver.current_allocation == target ->
          state

        receiver.current_allocation > target or not receiver.negotiable? ->
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
          |> update_prober_status(deficiency_allowed?: not receiver.negotiable?)

        true ->
          # Receiver raises its allocation. This might not be instantly granted
          state
          |> put_in([:track_receivers, pid], %{receiver | target_allocation: target})
          |> update_allocations()
          |> update_prober_status()
      end

    {:noreply, state}
  end

  @impl true
  def handle_info(:check_bits_sent, state) do
    # This callback implements probing to target.
    # It works in a very simple way. Periodically check the amount of data
    # sent by Track Receivers and compare it with the amount of data
    # that should have been sent up to this point.
    # If we didn't send enough, send enough probes to fill the missing part

    use Numbers, overload_operators: true

    probing_target =
      case state.prober_status do
        :maintain_estimation -> state.allocated_bandwidth
        :increase_estimation -> state.available_bandwidth + 200_000
        :allowed_bandwidth_deficiency -> state.allocated_bandwidth
        :disallowed_bandwidth_deficiency -> state.allocated_bandwidth
      end

    now = get_timestamp()
    elapsed_time_in_s = Time.as_seconds(now - state.estimation_timestamp)
    expected_bits = elapsed_time_in_s * probing_target
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
  defp stop_probing_timer(%__MODULE__{bitrate_timer: nil} = state), do: state

  defp stop_probing_timer(%__MODULE__{} = state) do
    {:ok, :cancel} = :timer.cancel(state.bitrate_timer)
    %{state | bitrate_timer: nil}
  end

  defp start_probing_timer(%__MODULE__{available_bandwidth: :unknown} = state), do: state

  defp start_probing_timer(%__MODULE__{} = state) do
    # we're not probing if we estimate infinite bandwidth or when none of the tracks is deficient
    {:ok, timer} = :timer.send_interval(10, :check_bits_sent)
    %{state | bitrate_timer: timer, estimation_timestamp: get_timestamp(), bits_sent: 0}
  end

  defp update_probing_statuses(state) do
    cond do
      not is_deficient?(state) and state.prober_status == :increase_estimation ->
        Logger.debug("Switching probing target to maintain estimation")

        state
        |> Map.put(:prober_status, :maintain_estimation)
        |> stop_probing_timer()
        |> start_probing_timer()

      state.prober_status == :maintain_estimation and is_deficient?(state) ->
        Logger.debug("Switching probing target to increase estimation")

        state
        |> Map.put(:prober_status, :increase_estimation)
        |> stop_probing_timer()
        |> start_probing_timer()

      true ->
        state
    end
  end

  defp update_deficiency_statuses(state, deficiency_allowed?) do
    deficiency_status =
      if deficiency_allowed?,
        do: :allowed_bandwidth_deficiency,
        else: :disallowed_bandwidth_deficiency

    cond do
      state.prober_status != deficiency_status and
          state.allocated_bandwidth > state.available_bandwidth ->
        Logger.debug("Switching prober state to #{deficiency_status}")

        state
        |> Map.put(:prober_status, deficiency_status)
        |> stop_probing_timer()
        |> start_probing_timer()

      state.prober_status in [:allowed_bandwidth_deficiency, :disallowed_bandwidth_deficiency] and
          state.allocated_bandwidth <= state.available_bandwidth ->
        new_status = if is_deficient?(state), do: :increase_estimation, else: :maintain_estimation

        Logger.debug(
          "Switching probing target to #{new_status} after being in the state of allowed deficiency"
        )

        state
        |> Map.put(:prober_status, new_status)
        |> stop_probing_timer()
        |> start_probing_timer()

      true ->
        state
    end
  end

  defp update_prober_status(state, options \\ []) do
    deficiency_allowed? = Keyword.get(options, :deficiency_allowed?, false)

    state
    |> update_deficiency_statuses(deficiency_allowed?)
    |> update_probing_statuses()
  end

  defp is_deficient?(state),
    do:
      state.track_receivers
      |> Map.values()
      |> Enum.any?(&(not is_nil(&1.target_allocation)))

  defp get_timestamp(), do: System.monotonic_time(:nanosecond)

  defp send_padding_packets(state, 0), do: state

  defp send_padding_packets(state, packets_num) do
    if Enum.empty?(state.probing_queue) do
      state
    else
      Enum.reduce(1..packets_num, state, fn _i, state ->
        # It's a good idea to select a track receiver in such a way that each one sends an equal amount of packets to create
        # => Round Robin
        {tr, queue} = Qex.pop!(state.probing_queue)
        send(tr, :send_padding_packet)

        %{state | probing_queue: Qex.push(queue, tr)}
      end)
    end
  end

  defp update_allocations(%__MODULE__{prober_status: :allowed_bandwidth_deficiency} = state),
    do: state

  defp update_allocations(%__MODULE__{available_bandwidth: :unknown} = state), do: state

  defp update_allocations(%__MODULE__{track_receivers: track_receivers} = state)
       when state.allocated_bandwidth > state.available_bandwidth do
    track_receivers
    |> Map.values()
    |> Enum.filter(& &1.negotiable?)
    |> case do
      [] ->
        nil

      negotiable_trs ->
        negotiable_trs
        |> Enum.sort_by(& &1.current_allocation)
        |> Enum.find(fn %{pid: pid} ->
          send(pid, :decrease_your_allocation)

          receive do
            {^pid, {:decrease_allocation_request, :accept}} ->
              true

            {^pid, {:decrease_allocation_request, :reject}} ->
              false

            {^pid, {:decrease_allocation_request, _other_reply}} ->
              raise ArgumentError,
                message: "Got illegal response from track receiver #{inspect(pid)}"
          after
            10 ->
              Logger.debug("Receiver #{inspect(pid)} didn't reply in time")
              false
          end
        end)
    end
    |> case do
      nil ->
        Logger.warn("We're overusing the bandwidth, but we cannot decrease our usage")

      value when is_map(value) ->
        :ok
    end

    state
  end

  defp update_allocations(%__MODULE__{available_bandwidth: bandwidth} = state) do
    free_bandwidth = bandwidth - state.allocated_bandwidth

    state.track_receivers
    |> Map.values()
    |> Enum.reject(&is_nil(&1.target_allocation))
    |> Enum.find(&(&1.target_allocation - &1.current_allocation <= free_bandwidth))
    |> case do
      nil ->
        state

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
