defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Test.EngineHelpers

  setup do
    extension_options = [
      vad: true
    ]

    network_options = [
      stun_servers: [
        %{server_addr: "stun.l.google.com", server_port: 19_302}
      ],
      turn_servers: []
    ]

    options = [
      id: "test_sfu",
      extension_options: extension_options,
      network_options: network_options
    ]

    {:ok, pid} = Engine.start_link(options, [])

    send(pid, {:register, self()})

    [sfu_engine: pid]
  end

  describe "joining to a room" do
    test "triggers :new_peer notification when media event is valid", %{sfu_engine: sfu_engine} do
      peer_id = "sample_id"

      metadata = %{
        "displayName" => "Bob"
      }

      tracks_metadata = [
        %{
          "type" => "audio",
          "source" => "microphone"
        },
        %{
          "type" => "video",
          "source" => "camera"
        }
      ]

      media_event =
        %{
          "type" => "join",
          "data" => %{
            "relayAudio" => true,
            "relayVideo" => true,
            "receiveMedia" => true,
            "metadata" => metadata,
            "tracksMetadata" => tracks_metadata
          }
        }
        |> Jason.encode!()

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata, ^tracks_metadata}}
    end
  end

  describe "accepting a new peer" do
    test "triggers peerAccepted event", %{sfu_engine: sfu_engine} do
      peer_id = "sample_id"

      metadata = %{
        "displayName" => "Bob"
      }

      tracks_metadata = [
        %{
          "type" => "audio",
          "source" => "microphone"
        },
        %{
          "type" => "video",
          "source" => "camera"
        }
      ]

      media_event =
        %{
          type: "join",
          data: %{
            relayAudio: true,
            relayVideo: true,
            receiveMedia: true,
            metadata: metadata,
            tracksMetadata: tracks_metadata
          }
        }
        |> Jason.encode!()

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata, ^tracks_metadata}}
      send(sfu_engine, {:accept_new_peer, peer_id})
      assert_receive {_from, {:sfu_media_event, ^peer_id, media_event}}

      assert %{"type" => "peerAccepted", "data" => %{"id" => peer_id, "peersInRoom" => []}} ==
               Jason.decode!(media_event)
    end
  end

  describe "denying a new peer" do
    test "triggers peerDenied event", %{sfu_engine: sfu_engine} do
      peer_id = "sample_id"

      metadata = %{
        "reason" => "bob smells"
      }

      tracks_metadata = [
        %{
          "type" => "audio",
          "source" => "microphone"
        },
        %{
          "type" => "video",
          "source" => "camera"
        }
      ]

      media_event =
        %{
          type: "join",
          data: %{
            relayAudio: true,
            relayVideo: true,
            receiveMedia: true,
            metadata: metadata,
            tracksMetadata: tracks_metadata
          }
        }
        |> Jason.encode!()

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata, ^tracks_metadata}}
      send(sfu_engine, {:deny_new_peer, peer_id, data: metadata})
      assert_receive {_from, {:sfu_media_event, ^peer_id, media_event}}

      assert %{"type" => "peerDenied", "data" => %{"reason" => "bob smells"}} ==
               Jason.decode!(media_event)
    end
  end

  describe "updating peer metadata" do
    setup do
      peer_id = "sample_id"

      state = %{
        id: "room_id",
        peers: %{
          peer_id => %{
            id: peer_id,
            metadata: %{"displayName" => "Bob"},
            mid_to_track_metadata: %{
              "ABC123" => %{"type" => "audio", "source" => "microphone"}
            }
          }
        },
        incoming_peers: %{},
        endpoints: %{},
        options: nil,
        packet_filters: %{}
      }

      EngineHelpers.register_test_pid()

      [peer_id: peer_id, state: state]
    end

    test "triggers peerUpdated event when metadata changes", %{
      peer_id: peer_id,
      state: initial_state
    } do
      media_event =
        %{
          type: "updatePeerMetadata",
          data: %{
            metadata: %{"displayName" => "Alice"}
          }
        }
        |> Jason.encode!()

      assert {{:ok, []}, new_state} =
               Engine.handle_other({:media_event, peer_id, media_event}, %{}, initial_state)

      assert new_state.peers == %{
               "sample_id" => %{
                 id: "sample_id",
                 metadata: %{"displayName" => "Alice"},
                 mid_to_track_metadata: %{
                   "ABC123" => %{"source" => "microphone", "type" => "audio"}
                 }
               }
             }

      assert new_media_event = EngineHelpers.wait_for_media_event_broadcast("peerUpdated")

      assert %{
               "type" => "peerUpdated",
               "data" => %{
                 "metadata" => %{"displayName" => "Alice"},
                 "midToTrackMetadata" => %{
                   "ABC123" => %{"source" => "microphone", "type" => "audio"}
                 },
                 "peerId" => peer_id
               }
             } ==
               new_media_event
    end

    test "does nothing when metadata does not change", %{peer_id: peer_id, state: initial_state} do
      media_event =
        %{
          type: "updatePeerMetadata",
          data: %{
            metadata: initial_state.peers[peer_id].metadata
          }
        }
        |> Jason.encode!()

      assert {{:ok, []}, _new_state} =
               Engine.handle_other({:media_event, peer_id, media_event}, %{}, initial_state)

      EngineHelpers.refute_media_event_broadcast("peerUpdated")
    end
  end

  describe "updating track metadata" do
    setup do
      peer_id = "sample_id"

      state = %{
        id: "room_id",
        peers: %{
          peer_id => %{
            id: peer_id,
            metadata: %{"displayName" => "Bob"},
            mid_to_track_metadata: %{
              "ABC123" => %{
                "type" => "audio"
              }
            }
          }
        },
        incoming_peers: %{},
        endpoints: %{},
        options: nil,
        packet_filters: %{}
      }

      EngineHelpers.register_test_pid()

      [peer_id: peer_id, state: state]
    end

    test "triggers peerUpdated event", %{peer_id: peer_id, state: initial_state} do
      media_event =
        %{
          type: "updateTracksMetadata",
          data: %{
            "midToTrackMetadata" => %{
              "ABC123" => %{
                "type" => "audio",
                "source" => "microphone"
              }
            }
          }
        }
        |> Jason.encode!()

      assert {{:ok, []}, new_state} =
               Engine.handle_other({:media_event, peer_id, media_event}, %{}, initial_state)

      assert new_state.peers == %{
               "sample_id" => %{
                 id: "sample_id",
                 metadata: %{"displayName" => "Bob"},
                 mid_to_track_metadata: %{
                   "ABC123" => %{"source" => "microphone", "type" => "audio"}
                 }
               }
             }

      assert new_media_event = EngineHelpers.wait_for_media_event_broadcast("peerUpdated")

      assert %{
               "type" => "peerUpdated",
               "data" => %{
                 "metadata" => %{"displayName" => "Bob"},
                 "midToTrackMetadata" => %{
                   "ABC123" => %{"source" => "microphone", "type" => "audio"}
                 },
                 "peerId" => peer_id
               }
             } ==
               new_media_event
    end

    test "does nothing when metadata does not change", %{peer_id: peer_id, state: initial_state} do
      media_event =
        %{
          type: "updateTracksMetadata",
          data: %{
            "midToTrackMetadata" => initial_state.peers[peer_id].mid_to_track_metadata
          }
        }
        |> Jason.encode!()

      assert {{:ok, []}, _new_state} =
               Engine.handle_other({:media_event, peer_id, media_event}, %{}, initial_state)

      EngineHelpers.refute_media_event_broadcast("peerUpdated")
    end
  end
end
