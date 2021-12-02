defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine

  setup do
    webrtc_extensions = [
      Membrane.WebRTC.Extension.VAD
    ]

    network_options = [
      stun_servers: [
        %{server_addr: "stun.l.google.com", server_port: 19_302}
      ],
      turn_servers: []
    ]

    options = [
      id: "test_sfu",
      webrtc_extensions: webrtc_extensions,
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

      media_event =
        %{
          "type" => "join",
          "data" => %{
            "receiveMedia" => true,
            "metadata" => metadata
          }
        }
        |> Jason.encode!()

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata}}
    end
  end

  # Fix me
  # Skipped due to bug occuring, when starting CNode in not distributed Erlang
  @tag :skip
  describe "accepting a new peer" do
    test "triggers peerAccepted event", %{sfu_engine: sfu_engine} do
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

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata}}
      send(sfu_engine, {:accept_new_peer, peer_id})
      assert_receive {_from, {:sfu_media_event, ^peer_id, media_event}}, 1000

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
    test "triggers peerDenied event", %{sfu_engine: sfu_engine} do
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

      send(sfu_engine, {:media_event, peer_id, media_event})
      assert_receive {_from, {:new_peer, ^peer_id, ^metadata}}
      send(sfu_engine, {:deny_new_peer, peer_id, data: metadata})
      assert_receive {_from, {:sfu_media_event, ^peer_id, media_event}}

      assert %{"type" => "peerDenied", "data" => %{"reason" => "bob smells"}} ==
               Jason.decode!(media_event)
    end
  end
end
