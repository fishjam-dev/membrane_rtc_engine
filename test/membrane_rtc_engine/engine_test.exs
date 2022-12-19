defmodule Membrane.RTC.EngineTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Message, Peer}

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

  @tag :skip
  describe "updating track metadata" do
    test "triggers trackUpdated event", %{rtc_engine: rtc_engine} do
      peer_id = "test_peer"
      metadata = %{"display_name" => "test_peer"}

      # FIXME: peer not added
      track_id = "test-track-id"

      metadata = %{"source" => "camera1"}

      track =
        Engine.Track.new(
          :video,
          "test-stream",
          peer_id,
          :VP8,
          nil,
          nil,
          id: track_id,
          metadata: metadata
        )

      endpoint = %TrackEndpoint{rtc_engine: rtc_engine, track: track}

      :ok = Engine.add_endpoint(rtc_engine, endpoint, peer_id: peer_id)

      assert_receive %Message.EndpointMessage{
        endpoint_id: :broadcast,
        message: {:media_event, data}
      }

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

      assert_receive %Message.EndpointMessage{
        endpoint_id: :broadcast,
        message: {:media_event, data}
      }

      assert %{
               "type" => "trackUpdated",
               "data" => %{
                 "peerId" => peer_id,
                 "metadata" => metadata,
                 "trackId" => track_id
               }
             } == Jason.decode!(data)
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
end
