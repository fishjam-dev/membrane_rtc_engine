defmodule Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionProber do
  @moduledoc false

  use GenServer

  alias Membrane.{Buffer, Time}

  @padding_packet_size Membrane.RTP.Packet.padding_packet_size()

  @spec start_link() :: GenServer.on_start()
  def start_link(), do: GenServer.start_link(__MODULE__, [], [])

  @impl GenServer
  def init(_opts) do
    state = %{
      bandwidth_estimation: nil,
      bitrate_timer: nil,
      estimation_timestamp: 0,
      bytes_sent: 0,
      track_receivers: Qex.new()
    }

    {:ok, state}
  end

  @impl GenServer
  def handle_cast({:bandwidth_estimation, estimation}, state) do
    if state.bitrate_timer, do: :timer.cancel(state.bitrate_timer)

    {:ok, timer} = :timer.send_interval(10, :check_bytes_sent)

    state = %{
      state
      | # TODO: rethink how target bandwidth should be calculated - + 200kbps seems potentially problematic
        bandwidth_estimation: (estimation + 200_000) / 8,
        estimation_timestamp: get_timestamp(),
        bitrate_timer: timer,
        bytes_sent: 0
    }

    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:buffer_sent, size}, state) do
    state = Map.update!(state, :bytes_sent, &(&1 + size))
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast(:probe_sent, state) do
    state = Map.update!(state, :bytes_sent, &(&1 + @padding_packet_size))
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast({:register_track_receiver, tr}, state) do
    state = Map.update!(state, :track_receivers, &Qex.push_front(&1, tr))
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:check_bytes_sent, state) do
    use Numbers, overload_operators: true

    now = get_timestamp()
    elapsed_time_in_s = Time.as_seconds(now - state.estimation_timestamp)
    expected_bytes = elapsed_time_in_s * state.bandwidth_estimation
    missing = expected_bytes - state.bytes_sent

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

  ## Public API

  @spec update_bandwidth_estimation(pid(), number()) :: :ok
  def update_bandwidth_estimation(prober, estimation),
    do: GenServer.cast(prober, {:bandwidth_estimation, estimation})

  @spec buffer_sent(pid(), Buffer.t()) :: :ok
  def buffer_sent(prober, %Buffer{payload: payload}),
    do: GenServer.cast(prober, {:buffer_sent, byte_size(payload)})

  @spec probe_sent(pid()) :: :ok
  def probe_sent(prober),
    do: GenServer.cast(prober, :probe_sent)

  @spec register_track_receiver(pid(), pid()) :: :ok
  def register_track_receiver(prober, tr \\ self()),
    do: GenServer.cast(prober, {:register_track_receiver, tr})

  ## Helper functions
  defp get_timestamp(), do: System.monotonic_time(:nanosecond)

  defp send_padding_packets(state, 0), do: state

  defp send_padding_packets(state, no_packets) do
    Enum.reduce(1..no_packets, state, fn _i, state ->
      # We need to select a track receiver in such a way that each one sends an equal amount of packets to create
      # => Round Robin
      {tr, track_receivers} = Qex.pop!(state.track_receivers)
      send(tr, :send_padding_packet)

      %{state | track_receivers: Qex.push(track_receivers, tr)}
    end)
  end
end
