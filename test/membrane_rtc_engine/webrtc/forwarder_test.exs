defmodule Membrane.RTC.Engine.Endpoint.WebRTC.ForwarderTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder

  test "Forwarder switches back to encoding being used before it became inactive" do
    forwarder = Forwarder.new(:VP8, 90_000, ["l", "m", "h"])

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "m", "h"]
           } = forwarder

    {forwarder, "m"} = Forwarder.encoding_inactive(forwarder, "h")

    assert %Forwarder{
             selected_encoding: nil,
             queued_encoding: "m",
             old_encoding: "h",
             active_encodings: ["l", "m"]
           } = forwarder

    {forwarder, "h"} = Forwarder.encoding_active(forwarder, "h")

    assert %Forwarder{
             selected_encoding: nil,
             queued_encoding: "h",
             old_encoding: nil,
             active_encodings: ["l", "m", "h"]
           } = forwarder
  end

  test "Forwarder doesn't switch to a new encoding when not currently used encoding is marked as inactive" do
    forwarder = Forwarder.new(:VP8, 90_000, ["l", "m", "h"])

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "m", "h"]
           } = forwarder

    {forwarder, nil} = Forwarder.encoding_inactive(forwarder, "m")

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "h"]
           } = forwarder
  end

  test "Forwarder switches to a new encoding when it is better than currently used encoding and while waiting for the actual encoding" do
    forwarder = Forwarder.new(:VP8, 90_000, ["l", "m", "h"])

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "m", "h"]
           } = forwarder

    {forwarder, nil} = Forwarder.encoding_inactive(forwarder, "m")

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "h"]
           } = forwarder

    {forwarder, "l"} = Forwarder.encoding_inactive(forwarder, "h")

    assert %Forwarder{
             selected_encoding: nil,
             queued_encoding: "l",
             old_encoding: "h",
             active_encodings: ["l"]
           } = forwarder

    {forwarder, "m"} = Forwarder.encoding_active(forwarder, "m")

    assert %Forwarder{
             selected_encoding: nil,
             queued_encoding: "m",
             old_encoding: "h",
             active_encodings: ["l", "m"]
           } = forwarder
  end
end
