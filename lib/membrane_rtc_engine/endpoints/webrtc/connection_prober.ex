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
    dbg(estimation)
    if state.bitrate_timer, do: :timer.cancel(state.bitrate_timer)

    {:ok, timer} = :timer.send_interval(100, :check_bytes_sent)

    state = %{
      state
      | bandwidth_estimation: estimation / 8,
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
    dbg({expected_bytes, missing})

    state =
      if missing > 0 do
        # TODO: consider holding out a little bit before sending a padding packet
        # Send paddings

        no_padding_packets =
          missing
          |> Ratio.new(@padding_packet_size)
          |> Ratio.ceil()
          |> then(&max(&1, Enum.count(state.track_receivers)))

        state
        |> send_padding_packets(no_padding_packets)
        |> Map.update!(:bytes_sent, &(&1 + no_padding_packets * @padding_packet_size))
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

  @spec register_track_receiver(pid(), pid()) :: :ok
  def register_track_receiver(prober, tr \\ self()),
    do: GenServer.cast(prober, {:register_track_receiver, tr})

  ## Helper functions
  defp get_timestamp(), do: System.monotonic_time(:nanosecond)

  defp send_padding_packets(state, 0), do: state

  defp send_padding_packets(state, no_packets) do
    Enum.reduce(1..no_packets, state, fn _i, state ->
      {tr, track_receivers} = Qex.pop!(state.track_receivers)
      send(tr, :send_padding_packet)

      %{state | track_receivers: Qex.push(track_receivers, tr)}
    end)
  end
end
