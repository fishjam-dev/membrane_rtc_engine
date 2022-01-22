defmodule Membrane.RTC.Engine.Endpoint.WebRTC.ForwarderTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.Forwarder

  test "Forwarder switches back to encoding being used before it became inactive" do
    forwarder = Forwarder.new(90)
    forwarder = Forwarder.encoding_inactive(forwarder, "h")

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: "l",
             old_encoding: "h",
             active_encodings: ["l"]
           } = forwarder

    forwarder = Forwarder.encoding_active(forwarder, "h")

    assert %Forwarder{
             selected_encoding: "h",
             queued_encoding: nil,
             old_encoding: nil,
             active_encodings: ["l", "h"]
           } = forwarder
  end
end
