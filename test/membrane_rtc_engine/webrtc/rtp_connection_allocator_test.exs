defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocatorTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.AllocationGrantedNotification
  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocator
  alias Membrane.Time

  @initial_bandwidth_estimation 100_000
  @padding_packet_size 8 * 256

  # Type describing probing scenario, to be used in `test_probing/1`
  @typep scenario_t() :: %{
           duration: Time.t(),
           expected_probing_rate: non_neg_integer(),
           on_start: (pid() -> :ok)
         }

  setup context do
    track = %{
      type: :video,
      variants:
        if(Map.get(context, :negotiable?, true),
          do: [:high, :medium, :low],
          else: [:high]
        )
    }

    [track: track]
  end

  describe "RTPConnectionAllocator" do
    setup do
      {:ok, prober} = RTPConnectionAllocator.create()

      RTPConnectionAllocator.update_bandwidth_estimation(prober, @initial_bandwidth_estimation)

      [prober: prober]
    end

    test "correctly probes the connection in :maintain_estimation state", %{
      prober: prober,
      track: track
    } do
      allocation = div(@initial_bandwidth_estimation, 2)

      scenario = [
        %{
          duration: Time.seconds(5),
          on_start: fn task ->
            send(task, {:request, allocation})

            assert_receive {:received, ^task,
                            %AllocationGrantedNotification{
                              allocation: ^allocation
                            }}
          end,
          expected_probing_rate: allocation
        }
      ]

      test_probing(prober, track, scenario)
    end

    test "correctly probes the connection in :maintain_estimation state, with probing target change",
         %{
           prober: prober,
           track: track
         } do
      allocation1 = div(@initial_bandwidth_estimation, 2)
      allocation2 = @initial_bandwidth_estimation

      scenario = [
        %{
          duration: Time.seconds(5),
          on_start: fn task ->
            send(task, {:request, allocation1})

            assert_receive {:received, ^task,
                            %AllocationGrantedNotification{
                              allocation: ^allocation1
                            }}
          end,
          expected_probing_rate: allocation1
        },
        %{
          duration: Time.seconds(5),
          on_start: fn task ->
            send(task, {:request, allocation2})

            assert_receive {:received, ^task,
                            %AllocationGrantedNotification{
                              allocation: ^allocation2
                            }}
          end,
          expected_probing_rate: allocation2
        }
      ]

      test_probing(prober, track, scenario)
    end

    test "correctly probes the connection in :increase_estimation state, with probing target change",
         %{
           prober: prober,
           track: track
         } do
      scenario = [
        %{
          duration: Time.seconds(5),
          on_start: fn task -> send(task, {:request, 999_999_999_999_999_999}) end,
          expected_probing_rate: @initial_bandwidth_estimation + 200_000
        },
        %{
          duration: Time.seconds(5),
          on_start: fn _task ->
            RTPConnectionAllocator.update_bandwidth_estimation(
              prober,
              @initial_bandwidth_estimation + 100_000
            )
          end,
          expected_probing_rate: @initial_bandwidth_estimation + 300_000
        }
      ]

      test_probing(prober, track, scenario)
    end

    test "Grants allocations if it has bandwidth for it", %{prober: prober, track: track} do
      allocation = 1_000
      RTPConnectionAllocator.register_track_receiver(prober, 0, track)
      RTPConnectionAllocator.request_allocation(prober, allocation)
      assert_receive %AllocationGrantedNotification{allocation: ^allocation}
    end

    test "Doesn't grant the allocation that it doesn't have the bandwidth for", %{
      prober: prober,
      track: track
    } do
      allocation = @initial_bandwidth_estimation * 2
      RTPConnectionAllocator.register_track_receiver(prober, 0, track)
      RTPConnectionAllocator.request_allocation(prober, allocation)
      refute_receive %AllocationGrantedNotification{allocation: ^allocation}
    end

    test "reallocates bandwidth of terminating TrackReceiver", %{track: track, prober: prober} do
      RTPConnectionAllocator.update_bandwidth_estimation(prober, 1000)

      tr1 = mock_track_receiver(prober, 500, track)
      tr2 = mock_track_receiver(prober, 500, track)
      Process.monitor(tr1)

      expected_allocation = 1000
      send(tr2, {:request, expected_allocation})
      refute_received {:received, ^tr2, %AllocationGrantedNotification{}}

      send(tr1, :terminate)
      assert_receive {:DOWN, _monitor, :process, ^tr1, _reason}

      assert_receive {:received, ^tr2,
                      %AllocationGrantedNotification{allocation: ^expected_allocation}}
    end

    test "reallocates bandwidth that was freed by another TrackReceiver", %{
      prober: prober,
      track: track
    } do
      RTPConnectionAllocator.update_bandwidth_estimation(prober, 1000)

      tr1 = mock_track_receiver(prober, 500, track)
      tr2 = mock_track_receiver(prober, 500, track)

      send(tr1, {:request, 1000})
      refute_received {:received, ^tr1, %AllocationGrantedNotification{}}

      send(tr2, {:request, 0})
      assert_receive {:received, ^tr2, %AllocationGrantedNotification{allocation: 0}}

      assert_receive {:received, ^tr1, %AllocationGrantedNotification{allocation: 1000}}
    end
  end

  describe "RTPConnectionAllocator prober state" do
    setup %{track: track} = _context do
      prober = RTPConnectionAllocator.create()

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:bandwidth_estimation, @initial_bandwidth_estimation},
          prober
        )

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:register_track_receiver, self(), 10_000, track, []},
          prober
        )

      [prober: prober]
    end

    test "switches to allowed_overuse when bandwidth estimation lowers below allocated bandwidth",
         %{prober: prober} do
      assert {:noreply, %{prober_status: :allowed_overuse} = prober} =
               RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 9000}, prober)

      assert {:noreply, %{prober_status: :allowed_overuse} = prober} =
               RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 9500}, prober)

      assert {:noreply, %{prober_status: :disallowed_overuse}} =
               RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 8500}, prober)
    end

    test "switches to disallowed_overuse from allowed_overuse when bandwidth allocation decreases",
         %{prober: prober} do
      assert {:noreply, %{prober_status: :allowed_overuse} = prober} =
               RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 9000}, prober)

      {:noreply, prober} = RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 0}, prober)
      assert prober.prober_status == :disallowed_overuse

      assert_receive :decrease_your_allocation, 0

      send(self(), {self(), {:decrease_allocation_request, :accept}})

      assert {:noreply, prober} =
               RTPConnectionAllocator.handle_cast({:request_allocation, self(), 0}, prober)

      assert prober.prober_status == :maintain_allocation
      refute_receive :decrease_your_allocation, 0
    end

    test "stays in allowed overuse if estimation increases", %{track: track, prober: prober} do
      # Get to allowed overuse
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:register_track_receiver, self(), 100_000_000, %{track | variants: [:high]}, []},
          prober
        )

      assert prober.prober_status == :allowed_overuse

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:bandwidth_estimation, prober.available_bandwidth + 200_000},
          prober
        )

      assert prober.prober_status == :allowed_overuse
    end

    test "switches to :increase_estimation upon allocation request and reverts back to :maintain_allocation after granting allocation",
         %{prober: prober} do
      request = 10_000_000

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:request_allocation, self(), request}, prober)

      assert prober.prober_status == :increase_estimation

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 2 * request}, prober)

      assert prober.prober_status == :maintain_allocation
      assert_receive %AllocationGrantedNotification{allocation: ^request}
    end

    @tag negotiable?: false
    test "switches to allowed overuse non-negotiable track raises allocation over available bandwidth",
         %{prober: prober} do
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:request_allocation, self(), 1_000_000_000}, prober)

      assert prober.prober_status == :allowed_overuse
    end

    test "switches to allowed overuse mode when new track receiver registers", %{
      track: track,
      prober: prober
    } do
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:register_track_receiver, self(), 1_000_000, track, []},
          prober
        )

      assert prober.prober_status == :allowed_overuse
    end
  end

  defp mock_track_receiver(
         prober,
         initial_allocation,
         track,
         negotiable? \\ true,
         probing? \\ false
       ) do
    owner = self()

    pid =
      spawn(fn ->
        :ok =
          RTPConnectionAllocator.register_track_receiver(prober, initial_allocation, track,
            negotiable?: negotiable?
          )

        send(owner, {:ready, self()})
        do_mock_track_receiver(owner, prober, probing?)
      end)

    receive do
      {:ready, ^pid} -> pid
    after
      100 -> raise "Mocking track receiver timed out"
    end
  end

  defp do_mock_track_receiver(owner, prober, probing?) do
    receive do
      :terminate ->
        :ok

      {:request, amount} ->
        RTPConnectionAllocator.request_allocation(prober, amount)
        do_mock_track_receiver(owner, prober, probing?)

      %AllocationGrantedNotification{} = msg ->
        send(owner, {:received, self(), msg})
        do_mock_track_receiver(owner, prober, probing?)

      :decrease_allocation_request = msg ->
        send(owner, {:received, self(), msg})
        do_mock_track_receiver(owner, prober, probing?)

      :send_padding_packet when probing? ->
        RTPConnectionAllocator.probe_sent(prober)
        send(owner, {:probe_sent, self()})
        do_mock_track_receiver(owner, prober, probing?)
    end
  end

  @spec test_probing(pid(), Track.t(), [scenario_t()]) :: :ok
  defp test_probing(connection_allocator, track, scenarios) do
    task = mock_track_receiver(connection_allocator, 0, track, true, true)
    task_monitor = Process.monitor(task)

    epochs =
      for scenario <- scenarios do
        scenario.on_start.(task)
        start = Time.monotonic_time()

        scenario.duration
        |> Time.as_milliseconds()
        |> Ratio.floor()
        |> Process.sleep()

        duration = Time.monotonic_time() - start
        {duration, scenario.expected_probing_rate}
      end

    send(task, {:request, 0})
    send(task, :terminate)
    assert_receive {:DOWN, ^task_monitor, :process, _pid, _reason}

    expected_amount_of_paddings =
      epochs
      |> Enum.reduce(0, fn {duration, rate}, acc ->
        duration
        |> Time.as_seconds()
        |> Ratio.mult(rate)
        |> Ratio.add(acc)
      end)
      |> Ratio.div(@padding_packet_size)
      |> Ratio.floor()

    if expected_amount_of_paddings > 1 do
      for i <- 1..(expected_amount_of_paddings - 1) do
        assert_receive {:probe_sent, ^task},
                       0,
                       "Only received #{i - 1}/#{expected_amount_of_paddings} paddings"
      end
    end

    for _i <- 1..2 do
      receive do
        {:probe_sent, ^task} -> :ok
      after
        0 -> :ok
      end
    end

    refute_receive {:probe_sent, ^task}, 0, "Received too many paddings!"
  end
end
