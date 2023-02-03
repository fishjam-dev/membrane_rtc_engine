defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Peer, Track}

  alias Membrane.RTC.Engine.Support.{
    MessageEndpoint,
    TrackEndpoint
  }

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    [rtc_engine: pid]
  end

  describe ":ready message" do
    test "triggers :new_peer", %{rtc_engine: rtc_engine} do
      endpoint_spec = %MessageEndpoint{rtc_engine: rtc_engine, owner: self()}

      Engine.add_endpoint(rtc_engine, endpoint_spec)
      Engine.add_endpoint(rtc_engine, endpoint_spec, peer_id: "peer")
      refute_receive {:new_peer, _peer}

      Engine.message_endpoint(
        rtc_engine,
        "peer",
        {:execute_actions, [notify: {:ready, "metadata"}]}
      )

      assert_receive {:new_peer, %Peer{id: "peer", metadata: "metadata"}}
      assert_receive {:ready, []}
    end

    test "is ignored for non-peers", %{rtc_engine: rtc_engine} do
      endpoint_spec = %MessageEndpoint{rtc_engine: rtc_engine, owner: self()}

      Engine.add_endpoint(rtc_engine, endpoint_spec)
      Engine.add_endpoint(rtc_engine, endpoint_spec, endpoint_id: "not-a-peer")

      Engine.message_endpoint(
        rtc_engine,
        "not-a-peer",
        {:execute_actions, [notify: {:ready, "metadata"}]}
      )

      refute_receive {:new_peer, _peer}
      refute_receive {:ready, _peers_in_room}
    end

    test "reports other peers", %{rtc_engine: rtc_engine} do
      peer1_spec = %TrackEndpoint{
        rtc_engine: rtc_engine,
        track: video_track("peer1", "track1", "track1-metadata", "stream1"),
        peer_metadata: "peer1-metadata"
      }

      msg_endpoint_spec = %MessageEndpoint{rtc_engine: rtc_engine, owner: self()}

      Engine.add_endpoint(rtc_engine, peer1_spec, peer_id: "peer1")
      Engine.add_endpoint(rtc_engine, msg_endpoint_spec, peer_id: "peer2")

      Engine.message_endpoint(
        rtc_engine,
        "peer2",
        {:execute_actions, [notify: {:ready, "metadata"}]}
      )

      assert_receive {:ready, peers_in_room}

      assert peers_in_room == [
               %{
                 id: "peer1",
                 metadata: "peer1-metadata",
                 trackIdToMetadata: %{"track1" => "track1-metadata"}
               }
             ]

      assert_receive {:new_tracks, [%Track{id: "track1"}]}
    end
  end

  describe ":update_track_metadata" do
    setup :setup_for_metadata_tests

    test "triggers :track_metadata_updated", %{
      rtc_engine: rtc_engine,
      track: %Track{id: track_id}
    } do
      Engine.message_endpoint(
        rtc_engine,
        "track-endpoint",
        {:execute_actions, [notify: {:update_track_metadata, track_id, "new-metadata"}]}
      )

      assert_receive {:track_metadata_updated, %Track{id: ^track_id, metadata: "new-metadata"}}
    end

    test "ignores identical metadata", %{rtc_engine: rtc_engine, track: track} do
      Engine.message_endpoint(
        rtc_engine,
        "track-endpoint",
        {:execute_actions, [notify: {:update_track_metadata, track.id, track.metadata}]}
      )

      refute_receive {:track_metadata_updated, _track}
    end
  end

  describe ":update_peer_metadata" do
    setup :setup_for_metadata_tests

    test "triggers :peer_metadata_updated", %{rtc_engine: rtc_engine, peer: %{id: peer_id}} do
      Engine.message_endpoint(
        rtc_engine,
        "track-endpoint",
        {:execute_actions, [notify: {:update_peer_metadata, "new-metadata"}]}
      )

      assert_receive {:peer_metadata_updated, %Peer{id: ^peer_id, metadata: "new-metadata"}}
    end

    test "ignores identical metadata", %{rtc_engine: rtc_engine, peer: peer} do
      Engine.message_endpoint(
        rtc_engine,
        "track-endpoint",
        {:execute_actions, [notify: {:update_peer_metadata, peer.metadata}]}
      )

      refute_receive {:peer_metadata_updated, _track}
    end
  end

  describe "Engine.message_endpoint/3" do
    test "forwards message to endpoint", %{rtc_engine: rtc_engine} do
      endpoint = %MessageEndpoint{rtc_engine: rtc_engine, owner: self()}
      endpoint_id = :test_endpoint
      :ok = Engine.add_endpoint(rtc_engine, endpoint, endpoint_id: endpoint_id)
      :ok = Engine.message_endpoint(rtc_engine, endpoint_id, :message)
      assert_receive(:message, 1_000)
    end

    test "does nothing when endpoint doesn't exist", %{rtc_engine: rtc_engine} do
      endpoint_id = :test_endpoint
      :ok = Engine.message_endpoint(rtc_engine, endpoint_id, :message)
      refute_receive :message
    end
  end

  test "engine sends EndpointCrashed message", %{rtc_engine: rtc_engine} do
    endpoint = %MessageEndpoint{rtc_engine: rtc_engine, owner: self()}
    endpoint_id = :test_endpoint
    :ok = Engine.add_endpoint(rtc_engine, endpoint, endpoint_id: endpoint_id)
    msg = {:execute_actions, [:some_invalid_action]}
    :ok = Engine.message_endpoint(rtc_engine, :test_endpoint, msg)
    assert_receive %Membrane.RTC.Engine.Message.EndpointCrashed{endpoint_id: :test_endpoint}
  end

  defp video_track(peer_id, track_id, metadata, stream_id \\ "test-stream") do
    Engine.Track.new(:video, stream_id, peer_id, :VP8, nil, nil,
      id: track_id,
      metadata: metadata
    )
  end

  defp setup_for_metadata_tests(%{rtc_engine: rtc_engine}) do
    track = video_track("track-endpoint", "track1", "track-metadata")

    peer = %Peer{
      id: "track-endpoint",
      metadata: "original-metadata"
    }

    track_endpoint = %TrackEndpoint{
      rtc_engine: rtc_engine,
      track: track,
      peer_metadata: peer.metadata
    }

    msg_endpoint = %MessageEndpoint{
      rtc_engine: rtc_engine,
      owner: self()
    }

    Engine.add_endpoint(rtc_engine, track_endpoint, peer_id: peer.id)
    Engine.add_endpoint(rtc_engine, msg_endpoint, endpoint_id: "message-endpoint")

    assert_receive {:new_tracks, [%Track{id: "track1"}]}
    assert :ok = Engine.subscribe(rtc_engine, "message-endpoint", "track1")

    Engine.message_endpoint(
      rtc_engine,
      "track-endpoint",
      {:execute_actions, [notify: {:track_ready, track.id, hd(track.variants), track.encoding}]}
    )

    [
      track: track,
      track_endpoint: track_endpoint,
      peer: peer
    ]
  end
end
