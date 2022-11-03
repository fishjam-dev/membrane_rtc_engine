defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantTrackerTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.VariantTracker

  test "VariantTracker detects that variant is no longer active" do
    tracker = VariantTracker.new(:high, 5, 10)

    # simulate we received only 4 packets from last check
    tracker =
      Enum.reduce(1..4, tracker, fn _sample, tracker ->
        VariantTracker.increment_samples(tracker)
      end)

    ret = VariantTracker.check_variant_status(tracker)

    assert {:status_changed, _tracker, :inactive} = ret
  end

  test "VariantTracker detects that variant is active again" do
    tracker = VariantTracker.new(:high, 5, 10)

    # simulate we received only 4 packets from last check
    tracker =
      Enum.reduce(1..4, tracker, fn _sample, tracker ->
        VariantTracker.increment_samples(tracker)
      end)

    ret = VariantTracker.check_variant_status(tracker)

    assert {:status_changed, tracker, :inactive} = ret

    # simulate we received 5 packets in subsequent 9 cycles
    tracker =
      Enum.reduce(1..9, tracker, fn _cycle, tracker ->
        tracker =
          Enum.reduce(1..5, tracker, fn _sample, tracker ->
            VariantTracker.increment_samples(tracker)
          end)

        assert {:ok, tracker} = VariantTracker.check_variant_status(tracker)
        tracker
      end)

    # simulate one more cycle
    tracker =
      Enum.reduce(1..5, tracker, fn _sample, tracker ->
        VariantTracker.increment_samples(tracker)
      end)

    # now VariantTracker should detect change
    assert {:status_changed, _tracker, :active} = VariantTracker.check_variant_status(tracker)
  end
end
