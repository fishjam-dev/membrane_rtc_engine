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
  #
  # - `:allowed_overuse` - we have allocated more bandwidth than the estimation, but the estimation
  #  is increasing. It's only possible to reach this state though allocations from non-negotiable Track
  #  Receivers and through new Track Receiver registering. All allocations will be frozen when
  #  RTPConnectionAllocator is in this state.
  # - `:disallowed_overuse` - we have allocated more bandwidth than estimated. This state can be reached
  #  when we're in `:allowed_overuse` and estimation decreased, or if the estimation decreases below
  #  the level of total allocation. We will aim to lower total allocation when operating in this state.
  #
  # `:increase estimation` and `:maintain_allocation` will be reffered to as "probing statuses",
  # while `:allowed_overuse` and `:disallowed_overuse`, as "overuse statuses"
  @typep prober_status_t() ::
           :increase_estimation
           | :maintain_allocation
           | :allowed_overuse
           | :disallowed_overuse

  @opaque t() :: %__MODULE__{
            track_receivers: %{pid() => track_receiver_metadata()},
            probing_queue: Qex.t(),
            prober_status: prober_status_t(),
            available_bandwidth: non_neg_integer() | :unknown,
            allocated_bandwidth: non_neg_integer(),
            probing_timer: :timer.tref() | nil,
            probing_epoch_start: integer(),
            bits_sent: non_neg_integer(),
            prev_probing_epochs_overflow: integer()
          }

  defstruct [
    :probing_timer,
    available_bandwidth: :unknown,
    prober_status: :allowed_overuse,
    track_receivers: %{},
    allocated_bandwidth: 0,
    probing_epoch_start: 0,
    bits_sent: 0,
    probing_queue: Qex.new(),
    prev_probing_epochs_overflow: 0
  ]

  @padding_packet_size 8 * 256

  @impl true
  def start_link(), do: GenServer.start_link(__MODULE__, [], [])

  ## Public API

  @impl true
  def register_track_receiver(prober, bandwidth, track, options \\ []),
    do: GenServer.cast(prober, {:register_track_receiver, self(), bandwidth, track, options})

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
  def set_negotiability_status(allocator, value),
    do: GenServer.cast(allocator, {:set_negotiability_status, self(), value})

  @impl true
  def init(_opts) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_cast({:bandwidth_estimation, estimation}, state) do
    Logger.info("Received bandwidth estimation of #{estimation / 1024} kbps")

    estimation_increasing? =
      state.available_bandwidth == :unknown or estimation >= state.available_bandwidth

    state =
      state
      |> Map.put(:available_bandwidth, estimation)
      # Allowed overuse status freezes all allocations
      # Check if we can change it before updating allocations
      # This function call also implements changing from :allowed_overuse
      # to :disallowed_overuse when estimation isn't increasing
      |> maybe_change_overuse_status(estimation_increasing?)
      |> update_allocations()
      # After updating the allocations, check probing statuses to check if
      # we're still deficient or in one of the overuse statuses
      |> maybe_change_probing_status()
      |> ensure_probing_timer_started()
      |> maybe_update_probing_target(state)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:bits_sent, size}, state) do
    state = Map.update!(state, :bits_sent, &(&1 + size))
    {:noreply, state}
  end

  @impl true
  def handle_cast({:register_track_receiver, pid, bandwidth, track, options}, state) do
    # This is the very first call that we're getting from the Track Receiver
    # It is already sending some variant, so whatever bandwidth they are using will be initially allocated
    # without question
    Process.monitor(pid)

    negotiable? =
      if Keyword.has_key?(options, :negotiable?),
        do: options[:negotiable?],
        else: length(track.variants) > 1

    receiver = %{
      pid: pid,
      current_allocation: bandwidth,
      target_allocation: nil,
      negotiable?: negotiable?
    }

    state =
      if track.type == :video do
        Map.update!(state, :probing_queue, &Qex.push(&1, pid))
      else
        state
      end
      |> put_in([:track_receivers, pid], receiver)
      |> Map.update!(:allocated_bandwidth, &(&1 + bandwidth))
      |> update_status(overuse_allowed?: true)
      |> maybe_update_probing_target(state)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_negotiability_status, pid, value}, state) do
    state =
      case state.track_receivers[pid] do
        nil ->
          Logger.error(
            "Attempted to change negotiability status of a Track Receiver that doesn't exist. Ignoring"
          )

          state

        %{negotiable?: ^value} ->
          Logger.debug(
            "Attempted to change negotiability status #{value}, but it is already in this state"
          )

          state

        _track_receiver ->
          Logger.debug(
            "Track Receiver #{inspect(pid)} requested to set its negotiability status to #{value}"
          )

          state
          |> put_in([:track_receivers, pid, :negotiable?], value)
          # We shouldn't go into allowed_overuse mode, but we also shouldn't swap disallowed_overuse for allowed_overuse
          |> maybe_change_overuse_status(state.prober_status != :disallowed_overuse)
          |> update_allocations()
          |> maybe_change_probing_status()
          |> maybe_update_probing_target(state)
      end

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
          |> update_status(overuse_allowed?: not receiver.negotiable?)

        true ->
          # Receiver raises its allocation. This might not be instantly granted
          state
          |> put_in([:track_receivers, pid], %{receiver | target_allocation: target})
          |> update_allocations()
          |> update_status()
      end
      |> maybe_update_probing_target(state)

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _monitor, :process, pid, _reason}, state) do
    # Track Receiver has been terminated or died
    # Regardless of the reason, let's free its allocation

    {tr_metadata, state} = pop_in(state, [:track_receivers, pid])

    Logger.debug(
      "Track Receiver #{inspect(pid)} has been removed. Freeing its allocation of #{tr_metadata.current_allocation / 1024} kbps"
    )

    state =
      state
      |> Map.update!(:allocated_bandwidth, &(&1 - tr_metadata.current_allocation))
      |> maybe_change_overuse_status(state.prober_status == :allowed_overuse)
      |> update_allocations()
      |> maybe_change_probing_status()
      |> maybe_update_probing_target(state)

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

    missing = expected_bits(state) - state.bits_sent

    state =
      if missing > 0 do
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

  @impl true
  def handle_info({_pid, {:decrease_allocation_request, _response}}, state) do
    {:noreply, state}
  end

  ## Helper functions
  defp ensure_probing_timer_started(%__MODULE__{probing_timer: probing_timer} = state)
       when not is_nil(probing_timer),
       do: state

  defp ensure_probing_timer_started(%__MODULE__{available_bandwidth: :unknown} = state), do: state

  defp ensure_probing_timer_started(%__MODULE__{} = state) do
    # we're not probing if we estimate infinite bandwidth or when none of the tracks is deficient
    {:ok, timer} = :timer.send_interval(10, :check_bits_sent)
    %{state | probing_timer: timer, probing_epoch_start: get_timestamp(), bits_sent: 0}
  end

  # Checks and updates probing statuses
  defp maybe_change_probing_status(state) do
    cond do
      state.available_bandwidth == :unknown ->
        state

      # Overuse states take priority over probing states
      state.prober_status in [:allowed_overuse, :disallowed_overuse] ->
        state

      not is_deficient?(state) and state.prober_status != :maintain_allocation ->
        Logger.debug("Switching prober state to maintain estimation")

        Map.put(state, :prober_status, :maintain_allocation)

      is_deficient?(state) and state.prober_status != :increase_estimation ->
        Logger.debug("Switching prober state to increase estimation")

        Map.put(state, :prober_status, :increase_estimation)

      true ->
        state
    end
  end

  # Checks and updates overuse statuses
  defp maybe_change_overuse_status(state, overuse_allowed?) do
    overuse_status =
      if overuse_allowed?,
        do: :allowed_overuse,
        else: :disallowed_overuse

    cond do
      state.available_bandwidth == :unknown ->
        state

      state.prober_status != overuse_status and
          state.allocated_bandwidth > state.available_bandwidth ->
        Logger.debug("Switching prober state to #{overuse_status}")

        Map.put(state, :prober_status, overuse_status)

      state.prober_status in [:allowed_overuse, :disallowed_overuse] and
          state.allocated_bandwidth <= state.available_bandwidth ->
        new_status = if is_deficient?(state), do: :increase_estimation, else: :maintain_allocation

        Logger.debug(
          "Switching prober state to #{new_status} after being in the state of #{state.prober_status}"
        )

        Map.put(state, :prober_status, new_status)

      true ->
        state
    end
  end

  defp update_status(state, options \\ []) do
    overuse_allowed? = Keyword.get(options, :overuse_allowed?, false)

    state
    |> maybe_change_overuse_status(overuse_allowed?)
    |> maybe_change_probing_status()
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

  defp update_allocations(%__MODULE__{prober_status: :allowed_overuse} = state),
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

  defp probing_target(state) do
    case state.prober_status do
      :maintain_allocation -> state.allocated_bandwidth
      :increase_estimation -> state.available_bandwidth + 200_000
      :allowed_overuse -> state.allocated_bandwidth
      :disallowed_overuse -> state.allocated_bandwidth
    end
  end

  defp expected_bits(state) do
    probing_target = probing_target(state)

    now = get_timestamp()
    elapsed_time_in_s = Time.as_seconds(now - state.probing_epoch_start)

    elapsed_time_in_s
    |> Ratio.mult(probing_target)
    |> Ratio.add(state.prev_probing_epochs_overflow)
    |> Ratio.floor()
  end

  defp maybe_update_probing_target(new_state, old_state) do
    target_updated? = probing_target(new_state) != probing_target(old_state)

    if target_updated? do
      %{
        new_state
        | prev_probing_epochs_overflow: expected_bits(old_state) - new_state.bits_sent,
          bits_sent: 0,
          probing_epoch_start: get_timestamp()
      }
    else
      new_state
    end
  end
end
