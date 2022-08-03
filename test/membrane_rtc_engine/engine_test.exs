defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Message, Peer}
  alias Membrane.RTC.Engine.Support.{MessageEndpoint, TrackEndpoint}

  setup do
    options = [
      id: "test_rtc"
    ]

    {:ok, pid} = Engine.start_link(options, [])

    Engine.register(pid, self())

    [rtc_engine: pid]
  end

  describe "joining to a room" do
    test "triggers :new_peer notification when media event is valid", %{rtc_engine: rtc_engine} do
      peer_id = "sample_id"

      metadata = %{
        "displayName" => "Bob"
      }

      media_event =
        %{
          "type" => "join",
          "data" => %{
            "receiveMedia" => true,
            "metadata" => metadata
          }
        }
        |> Jason.encode!()

      peer = %Peer{
        id: peer_id,
        metadata: metadata
      }

      Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})
      assert_receive %Message.NewPeer{rtc_engine: ^rtc_engine, peer: ^peer}
    end
  end

  # Fix me
  # Skipped due to bug occuring, when starting CNode in not distributed Erlang
  @tag :skip
  describe "accepting a new peer" do
    test "triggers peerAccepted event", %{rtc_engine: rtc_engine} do
      peer_id = "sample_id"

      metadata = %{
        "displayName" => "Bob"
      }

      media_event =
        %{
          type: "join",
          data: %{
            receiveMedia: true,
            metadata: metadata
          }
        }
        |> Jason.encode!()

      Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata}}
      Engine.accept_peer(rtc_engine, peer_id)
      assert_receive {_from, {:rtc_media_event, ^peer_id, media_event}}, 1000

      assert %{
               "type" => "peerAccepted",
               "data" => %{
                 "id" => peer_id,
                 "peersInRoom" => [],
                 "iceTransportPolicy" => "all",
                 "integratedTurnServers" => []
               }
             } ==
               Jason.decode!(media_event)
    end
  end

  describe "denying a new peer" do
    test "triggers peerDenied event", %{rtc_engine: rtc_engine} do
      peer_id = "sample_id"

      metadata = %{
        "reason" => "bob smells"
      }

      media_event =
        %{
          type: "join",
          data: %{
            receiveMedia: true,
            metadata: metadata
          }
        }
        |> Jason.encode!()

      peer = %Peer{
        id: peer_id,
        metadata: metadata
      }

      Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})
      assert_receive %Message.NewPeer{rtc_engine: ^rtc_engine, peer: ^peer}
      Engine.deny_peer(rtc_engine, peer_id, data: metadata)
      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: ^peer_id, data: data}

      assert %{"type" => "peerDenied", "data" => %{"reason" => "bob smells"}} ==
               Jason.decode!(data)
    end
  end

  describe "adding a new peer" do
    test "triggers peerAccepted and peerJoined events", %{rtc_engine: rtc_engine} do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}
      add_peer(rtc_engine, peer_id, metadata)
    end
  end

  describe "updating peer metadata" do
    test "triggers peerUpdated event", %{rtc_engine: rtc_engine} do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}
      add_peer(rtc_engine, peer_id, metadata)

      media_event =
        %{
          type: "updatePeerMetadata",
          data: %{
            metadata: %{"info" => "test"}
          }
        }
        |> Jason.encode!()

      :ok = Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "peerUpdated",
               "data" => %{
                 "peerId" => "test_peer",
                 "metadata" => %{"info" => "test"}
               }
             } == Jason.decode!(data)
    end

    test "doesn't trigger peerUpdated event, when metadata doesn't differ", %{
      rtc_engine: rtc_engine
    } do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}
      add_peer(rtc_engine, peer_id, metadata)

      media_event =
        %{
          type: "updatePeerMetadata",
          data: %{
            metadata: metadata
          }
        }
        |> Jason.encode!()

      :ok = Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})

      refute_receive(
        %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: _media_event},
        1000
      )
    end
  end

  describe "updating track metadata" do
    test "triggers trackUpdated event", %{rtc_engine: rtc_engine} do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}

      add_peer(rtc_engine, peer_id, metadata)
      track_id = "test-track-id"

      metadata = %{"source" => "camera1"}

      track =
        Engine.Track.new(
          :video,
          "test-stream",
          peer_id,
          :VP8,
          nil,
          :raw,
          nil,
          id: track_id,
          metadata: metadata
        )

      endpoint = %TrackEndpoint{rtc_engine: rtc_engine, track: track}

      :ok = Engine.add_endpoint(rtc_engine, endpoint, peer_id: peer_id)

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "tracksAdded",
               "data" => %{"trackIdToMetadata" => %{track_id => metadata}, "peerId" => peer_id}
             } == Jason.decode!(data)

      metadata = %{"source" => "camera2"}

      media_event =
        %{
          type: "updateTrackMetadata",
          data: %{
            "trackId" => track_id,
            "trackMetadata" => metadata
          }
        }
        |> Jason.encode!()

      :ok = Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "trackUpdated",
               "data" => %{
                 "peerId" => peer_id,
                 "metadata" => metadata,
                 "trackId" => track_id
               }
             } == Jason.decode!(data)
    end

    test "doesn't trigger trackUpdated event, when metadata doesn't differ", %{
      rtc_engine: rtc_engine
    } do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}

      add_peer(rtc_engine, peer_id, metadata)
      track_id = "test-track-id"

      metadata = %{"source" => "camera1"}

      track =
        Engine.Track.new(
          :video,
          "test-stream",
          peer_id,
          :VP8,
          nil,
          :raw,
          nil,
          id: track_id,
          metadata: metadata
        )

      endpoint = %TrackEndpoint{rtc_engine: rtc_engine, track: track}

      :ok = Engine.add_endpoint(rtc_engine, endpoint, peer_id: peer_id)

      assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

      assert %{
               "type" => "tracksAdded",
               "data" => %{"trackIdToMetadata" => %{track_id => metadata}, "peerId" => peer_id}
             } == Jason.decode!(data)

      media_event =
        %{
          type: "updateTrackMetadata",
          data: %{
            "trackId" => track_id,
            "trackMetadata" => track.metadata
          }
        }
        |> Jason.encode!()

      :ok = Engine.receive_media_event(rtc_engine, {:media_event, peer_id, media_event})

      refute_receive(
        %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: _media_event},
        1000
      )
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

  defp add_peer(rtc_engine, peer_id, metadata) do
    peer = Peer.new(peer_id, metadata)
    :ok = Engine.add_peer(rtc_engine, peer)
    assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: ^peer_id, data: data}

    assert %{"type" => "peerAccepted", "data" => %{"id" => peer_id, "peersInRoom" => []}} ==
             Jason.decode!(data)

    assert_receive %Message.MediaEvent{rtc_engine: ^rtc_engine, to: :broadcast, data: data}

    assert %{
             "type" => "peerJoined",
             "data" => %{
               "peer" => %{"id" => peer_id, "metadata" => metadata}
             }
           } == Jason.decode!(data)
  end
end
