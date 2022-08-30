defmodule Membrane.RTC.Engine.Endpoint.WebRTC.EncodingSelectorTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.EncodingSelector

  test "EncodingSelector selects another encoding when currently used encoding becomes inactive" do
    selector = create_selector()
    assert {selector, "m"} = EncodingSelector.encoding_inactive(selector, "h")
    assert {_selector, "l"} = EncodingSelector.encoding_inactive(selector, "m")
  end

  test "EncodingSelector selects encoding being used before it became inactive" do
    selector = create_selector()

    assert {selector, "m"} = EncodingSelector.encoding_inactive(selector, "h")

    selector = EncodingSelector.current_encoding(selector, "m")

    assert {_selector, "h"} = EncodingSelector.encoding_active(selector, "h")
  end

  test "target_encoding/2 sets target encoding and chooses it when it is active" do
    selector = create_selector()

    assert {selector, "m"} = EncodingSelector.encoding_inactive(selector, "h")
    assert {selector, "m"} = EncodingSelector.target_encoding(selector, "m")

    selector = EncodingSelector.current_encoding(selector, "m")

    # assert that encoding selector doesn't request the new encoding
    # even though it is better
    assert {_selector, nil} = EncodingSelector.encoding_active(selector, "h")
  end

  test "target_encoding/2 sets target encoding but doesn't switch to it when it is inactive" do
    selector = create_selector()

    assert {selector, nil} = EncodingSelector.encoding_inactive(selector, "l")
    assert {selector, nil} = EncodingSelector.target_encoding(selector, "l")
    assert {_selector, "l"} = EncodingSelector.encoding_active(selector, "l")
  end

  test "target_encoding/2 ignores non-existing encoding" do
    selector = create_selector()

    non_existing_encoding = "non-existing-encoding"

    assert {selector, nil} = EncodingSelector.target_encoding(selector, non_existing_encoding)
    assert selector.target_encoding != non_existing_encoding
  end

  test "EncodingSelector doesn't select a new encoding when not currently used encoding is marked as inactive" do
    selector = create_selector()

    assert {selector, nil} = EncodingSelector.encoding_inactive(selector, "m")
    assert {_selector, nil} = EncodingSelector.encoding_inactive(selector, "l")
  end

  test "EncodingSelector selects a new encoding when it is better than currently used encoding and while waiting for the target encoding" do
    selector = EncodingSelector.new(["h", "m", "l"], "h")

    assert {selector, "l"} = EncodingSelector.encoding_active(selector, "l")
    assert {selector, "m"} = EncodingSelector.encoding_active(selector, "m")
    assert {_selector, "h"} = EncodingSelector.encoding_active(selector, "h")
  end

  defp create_selector() do
    selector = EncodingSelector.new(["h", "m", "l"], "h")

    assert {selector, "h"} = EncodingSelector.encoding_active(selector, "h")

    selector = EncodingSelector.current_encoding(selector, "h")

    assert {selector, nil} = EncodingSelector.encoding_active(selector, "m")
    assert {selector, nil} = EncodingSelector.encoding_active(selector, "l")
    selector
  end
end
