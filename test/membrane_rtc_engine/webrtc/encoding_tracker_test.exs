defmodule Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTrackerTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTracker

  test "EncodingTracker detects that encoding is no longer active" do
    tracker = EncodingTracker.new("h", 5, 10)

    # simulate we received only 4 packets from last check
    tracker =
      Enum.reduce(1..4, tracker, fn _sample, tracker ->
        EncodingTracker.increment_samples(tracker)
      end)

    ret = EncodingTracker.check_encoding_status(tracker)

    assert {:status_changed, _tracker, :inactive} = ret
  end

  test "EncodingTracker detects that encoding is active again" do
    tracker = EncodingTracker.new("h", 5, 10)

    # simulate we received only 4 packets from last check
    tracker =
      Enum.reduce(1..4, tracker, fn _sample, tracker ->
        EncodingTracker.increment_samples(tracker)
      end)

    ret = EncodingTracker.check_encoding_status(tracker)

    assert {:status_changed, tracker, :inactive} = ret

    # simulate we received 5 packets in subsequent 9 cycles
    tracker =
      Enum.reduce(1..9, tracker, fn _cycle, tracker ->
        tracker =
          Enum.reduce(1..5, tracker, fn _sample, tracker ->
            EncodingTracker.increment_samples(tracker)
          end)

        assert {:ok, tracker} = EncodingTracker.check_encoding_status(tracker)
        tracker
      end)

    # simulate one more cycle
    tracker =
      Enum.reduce(1..5, tracker, fn _sample, tracker ->
        EncodingTracker.increment_samples(tracker)
      end)

    # now EncodingTracker should detect change
    assert {:status_changed, _tracker, :active} = EncodingTracker.check_encoding_status(tracker)
  end
end
