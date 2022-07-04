defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Message, Peer}

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

      state = %{
        network_options: [
          stun_servers: [
            %{server_addr: "stun.l.google.com", server_port: 19_302}
          ],
          turn_servers: [],
          dtls_pkey: Application.get_env(:membrane_videoroom_demo, :dtls_pkey),
          dtls_cert: Application.get_env(:membrane_videoroom_demo, :dtls_cert)
        ]
      }

      handshake_opts =
        if state.network_options[:dtls_pkey] &&
             state.network_options[:dtls_cert] do
          [
            client_mode: false,
            dtls_srtp: true,
            pkey: state.network_options[:dtls_pkey],
            cert: state.network_options[:dtls_cert]
          ]
        else
          [
            client_mode: false,
            dtls_srtp: true
          ]
        end

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

    test "same metadata doesn't trigger peerUpdated event", %{rtc_engine: rtc_engine} do
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

      receive do
        msg -> assert(msg == "")
      after
        500 ->
          assert(true)
      end
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
