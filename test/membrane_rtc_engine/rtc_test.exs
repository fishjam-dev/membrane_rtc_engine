defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Message, Peer}
  alias Membrane.RTC.Engine.Endpoint.WebRTC

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
      id: "test_rtc",
      webrtc_extensions: webrtc_extensions,
      network_options: network_options
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

      bin = %WebRTC{
        ice_name: peer_id,
        owner: self(),
        stun_servers: [],
        turn_servers: [],
        handshake_opts: handshake_opts,
        log_metadata: [peer_id: peer_id],
        filter_codecs: fn {rtp, fmtp} ->
          case rtp.encoding do
            "opus" -> true
            "H264" -> fmtp.profile_level_id === 0x42E01F
            _unsupported_codec -> false
          end
        end
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
end
