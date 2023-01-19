defmodule Membrane.RTC.Engine.Endpoint.WebRTC.MediaEventTest do
  use ExUnit.Case, async: true

  # This module tests if we correctly create structures for protobuf generation
  # No asserts are needed, as MediaEvent.encode/1 throws an error
  # when it encounters data that doesn't match the proto

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.WebRTC.MediaEvent

  test "peer_accepted/1" do
    "test" |> MediaEvent.peer_accepted() |> MediaEvent.encode()
  end

  test "peer_joined/1" do
    peer = %Engine.Peer{
      id: "test",
      metadata: "test"
    }

    peer |> MediaEvent.peer_joined() |> MediaEvent.encode()
  end

  test "peer_updated/1" do
    peer = %Engine.Peer{
      id: "test",
      metadata: "test"
    }

    peer |> MediaEvent.peer_updated() |> MediaEvent.encode()
  end

  test "track_added/1" do
    track = Engine.Track.new("video", "stream", "me", :high, 90_000, [], id: "test")
    track |> MediaEvent.track_added() |> MediaEvent.encode()
  end

  test "track_updated/2" do
    "test" |> MediaEvent.track_updated("test") |> MediaEvent.encode()
  end

  test "track_removed/1" do
    "test" |> MediaEvent.track_removed() |> MediaEvent.encode()
  end

  test "encoding_switched/3" do
    MediaEvent.encoding_switched("test", "h", :other) |> MediaEvent.encode()
  end

  test "sdp_answer/2" do
    mid_to_track_id = %{
      "1" => "1",
      "2" => "2"
    }

    MediaEvent.sdp_answer("sdp", mid_to_track_id) |> MediaEvent.encode()
  end

  test "offer_data/2" do
    turns = [
      %{
        mocked_server_addr: {127, 0, 0, 1},
        password: "1juNviMX72EOlI1DDY0gBsxEa1g=",
        pid: self(),
        relay_type: :udp,
        server_addr: {127, 0, 0, 1},
        server_port: 50_250,
        username: "1674114479:61f7346f-e4a3-466f-baaa-44bf0354bd51"
      }
    ]

    tracks = %{
      audio: 1,
      video: 1
    }

    MediaEvent.offer_data(tracks, turns) |> MediaEvent.encode()
  end

  test "candidate/2" do
    MediaEvent.candidate("candidate", 2) |> MediaEvent.encode()
  end

  test "sdp_offer/1" do
    MediaEvent.sdp_offer("sdp") |> MediaEvent.encode()
  end

  test "voice_activity/2" do
    MediaEvent.voice_activity("track", :speech) |> MediaEvent.encode()
    MediaEvent.voice_activity("track", :silence) |> MediaEvent.encode()
  end

  test "bandwidth_estimation/1" do
    MediaEvent.bandwidth_estimation(11_111) |> MediaEvent.encode()
  end
end
