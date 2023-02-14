defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantTrackerTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.VariantTracker

  test "VariantTracker.new/1" do
    assert %VariantTracker{} = VariantTracker.new(:high)
  end

  test "VariantTracker detects that variant is no longer active" do
    tracker = VariantTracker.new(:high)
    simulate_inactive_sequence(tracker)
  end

  test "VariantTracker detects that variant is active again" do
    tracker = VariantTracker.new(:high)

    tracker = simulate_inactive_sequence(tracker)
    simulate_active_sequence(tracker)
  end

  test "VariantTracker correctly classifies the track that goes in and out of activity" do
    :high
    |> VariantTracker.new()
    |> simulate_inactive_sequence()
    |> simulate_active_sequence()
    |> simulate_inactive_sequence()
    |> simulate_active_sequence()
  end

  defp simulate_inactive_sequence(tracker), do: apply_sequence(tracker, 1, 4, :inactive)
  defp simulate_active_sequence(tracker), do: apply_sequence(tracker, 10, 5, :active)

  defp apply_sequence(tracker, cycles, samples_per_cycle, desired_status) do
    refute tracker.status == desired_status

    cycle =
      if samples_per_cycle == 0,
        do: fn tracker -> tracker end,
        else: fn tracker ->
          Enum.reduce(1..samples_per_cycle, tracker, fn _sample, tracker ->
            VariantTracker.increment_samples(tracker)
          end)
        end

    tracker =
      if cycles > 1 do
        Enum.reduce(1..(cycles - 1), tracker, fn _sample, tracker ->
          tracker = cycle.(tracker)

          assert {:ok, tracker} = VariantTracker.check_variant_status(tracker)
          tracker
        end)
      else
        tracker
      end

    # simulate one more cycle
    tracker = cycle.(tracker)

    # now VariantTracker should detect change
    assert {:status_changed, tracker, ^desired_status} =
             VariantTracker.check_variant_status(tracker)

    tracker
  end
end
