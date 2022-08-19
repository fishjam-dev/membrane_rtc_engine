defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSenderStateTest do
  use ExUnit.Case, async: true

  import Membrane.RTC.Engine.Support.Utils

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSenderState
  alias Membrane.RTC.Engine.Track

  test "TrackSender marks each buffer with `is_keyframe` flag" do
    track = Track.new(:video, "stream1", "generated", :H264, 90_000, :raw, nil)
    test_is_keyframe(track, false)

    track = Track.new(:video, "stream1", "generated", :VP8, 90_000, :raw, nil)
    test_is_keyframe(track, false)

    track = Track.new(:video, "stream1", "generated", :OPUS, 48_000, :raw, nil)
    test_is_keyframe(track, true)
  end

  defp test_is_keyframe(track, expected_is_keyframe) do
    state = TrackSenderState.new(track)
    buffer = %Membrane.Buffer{payload: <<1, 2, 3, 4, 5>>}
    {_state, actions} = TrackSenderState.process(state, buffer)

    assert [buffer: {:output, buffer}] = actions
    assert %{is_keyframe: is_keyframe} = buffer.metadata
    assert expected_is_keyframe == is_keyframe
  end
end
