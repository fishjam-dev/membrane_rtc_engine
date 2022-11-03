defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocatorTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.AllocationGrantedNotification
  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPConnectionAllocator
  alias Membrane.Time

  @initial_bandwidth_estimation 10_000
  @padding_packet_size 8 * 256

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
      {:ok, prober} = RTPConnectionAllocator.start_link()

      RTPConnectionAllocator.update_bandwidth_estimation(prober, @initial_bandwidth_estimation)

      [prober: prober]
    end

    # This test is a little flaky
    test "correctly probes the connection in :maintain_allocation state", %{
      prober: prober,
      track: track
    } do
      requested_bandwidth = @initial_bandwidth_estimation
      my_pid = self()

      task =
        spawn(fn ->
          RTPConnectionAllocator.register_track_receiver(prober, 0, track)
          RTPConnectionAllocator.request_allocation(prober, requested_bandwidth)
          reply_to_send_padding_messages(my_pid, prober)
          RTPConnectionAllocator.request_allocation(prober, 0)
        end)

      Process.sleep(100)
      clear_inbox()

      start_time = Time.monotonic_time()
      Process.sleep(5_000)
      send(task, :die)

      duration = Time.monotonic_time() - start_time
      duration_in_s = Ratio.to_float(Time.as_seconds(duration))

      expected_amount_of_paddings =
        Ratio.new(requested_bandwidth * duration_in_s, @padding_packet_size)
        |> Ratio.to_float()
        |> floor()
        |> tap(&assert &1 > 1)

      for i <- 1..expected_amount_of_paddings do
        assert_receive :send_padding_packet,
                       0,
                       "Only received #{i - 1}/#{expected_amount_of_paddings}"

        :ok
      end

      receive do
        :send_padding_packet -> :ok
      after
        0 -> :ok
      end

      refute_receive :send_padding_packet, 0
    end

    # this test is a little flaky
    test "correctly probes the connection in :increase_allocation state", %{
      prober: prober,
      track: track
    } do
      expected_probing_rate = @initial_bandwidth_estimation + 200_000
      my_pid = self()

      task =
        spawn(fn ->
          RTPConnectionAllocator.register_track_receiver(prober, 0, track)
          RTPConnectionAllocator.request_allocation(prober, 999_999_999_999_999_999)
          reply_to_send_padding_messages(my_pid, prober)
          RTPConnectionAllocator.request_allocation(prober, 0)
        end)

      Process.sleep(100)
      clear_inbox()

      start_time = Time.monotonic_time()
      Process.sleep(5_000)
      send(task, :die)

      duration = Time.monotonic_time() - start_time
      duration_in_s = Ratio.to_float(Time.as_seconds(duration))

      expected_amount_of_paddings =
        Ratio.new(expected_probing_rate * duration_in_s, @padding_packet_size)
        |> Ratio.to_float()
        |> floor()
        |> tap(&assert &1 > 1)

      for i <- 1..expected_amount_of_paddings do
        assert_receive :send_padding_packet,
                       0,
                       "Only received #{i - 1}/#{expected_amount_of_paddings}"

        :ok
      end

      receive do
        :send_padding_packet -> :ok
      after
        0 -> :ok
      end

      refute_receive :send_padding_packet, 0
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
      allocation = 9_999_999_999_999_999
      RTPConnectionAllocator.register_track_receiver(prober, 0, track)
      RTPConnectionAllocator.request_allocation(prober, allocation)
      refute_receive %AllocationGrantedNotification{allocation: ^allocation}
    end
  end

  describe "RTPConnectionAllocator prober state" do
    setup %{track: track} = _context do
      prober = %RTPConnectionAllocator{}

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:bandwidth_estimation, @initial_bandwidth_estimation},
          prober
        )

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:hello, self(), 10_000, track}, prober)

      [prober: prober]
    end

    test "switches to disallowed_overuse when bandwidth estimation lowers below allocated bandwidth",
         %{prober: prober} do
      assert {:noreply, prober} =
               RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 0}, prober)

      assert prober.prober_status == :disallowed_overuse
      assert_receive :decrease_your_allocation, 0

      send(self(), {self(), {:decrease_allocation_request, :accept}})

      assert {:noreply, prober} =
               RTPConnectionAllocator.handle_cast({:request_allocation, self(), 0}, prober)

      assert prober.prober_status == :maintain_estimation
      refute_receive :decrease_your_allocation, 0
    end

    test "switches to disallowed_overuse from allowed_overuse when bandwidth allocation decreases",
         %{prober: prober, track: track} do
      # Get to allowed overuse
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:hello, self(), 100_000_000, %{track | variants: [:high]}},
          prober
        )

      assert prober.prober_status == :allowed_overuse

      {:noreply, prober} = RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 0}, prober)
      assert prober.prober_status == :disallowed_overuse
    end

    test "stays in allowed overuse if estimation increases", %{track: track, prober: prober} do
      # Get to allowed overuse
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast(
          {:hello, self(), 100_000_000, %{track | variants: [:high]}},
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

    test "switches to :increase_estimation upon allocation request and reverts back to :maintain_estimation after granting allocation",
         %{prober: prober} do
      request = 10_000_000

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:request_allocation, self(), request}, prober)

      assert prober.prober_status == :increase_estimation

      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:bandwidth_estimation, 2 * request}, prober)

      assert prober.prober_status == :maintain_estimation
      assert_receive %AllocationGrantedNotification{allocation: ^request}
    end

    @tag negotiable?: false
    test "switches to allowed overuse non-negotiable track raises allocation over available bandwidth",
         %{prober: prober} do
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:request_allocation, self(), 1_000_000_000}, prober)

      assert prober.prober_status == :allowed_overuse
    end

    test "switches to allowed overuse mode when new track receiver shows up", %{
      track: track,
      prober: prober
    } do
      {:noreply, prober} =
        RTPConnectionAllocator.handle_cast({:hello, self(), 1_000_000, track}, prober)

      assert prober.prober_status == :allowed_overuse
    end
  end

  defp clear_inbox(acc \\ 0) do
    receive do
      _message ->
        clear_inbox(acc + 1)
    after
      0 ->
        acc
    end
  end

  defp reply_to_send_padding_messages(parent, prober) do
    receive do
      :send_padding_packet ->
        RTPConnectionAllocator.probe_sent(prober)
        send(parent, :send_padding_packet)
        reply_to_send_padding_messages(parent, prober)

      :die ->
        :ok
    end
  end
end
