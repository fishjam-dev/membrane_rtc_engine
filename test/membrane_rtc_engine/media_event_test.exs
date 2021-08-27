defmodule Membrane.RTC.Engine.MediaEventTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.MediaEvent

  describe "create_peer_joined_event" do
    test "" do
      peer_id = "abc123"
      metadata = %{location: "Krawów"}
      mid_to_track_metadata = %{"ABCDEF1234" => %{}}

      assert {:sfu_media_event, :broadcast, data} =
               MediaEvent.create_peer_joined_event(peer_id, metadata, mid_to_track_metadata)

      assert Jason.decode!(data) == %{
               "data" => %{
                 "peer" => %{
                   "id" => "abc123",
                   "metadata" => %{"location" => "Krawów"},
                   "midToTrackMetadata" => %{"ABCDEF1234" => %{}}
                 }
               },
               "type" => "peerJoined"
             }
    end
  end

  describe "create_peer_updated_event" do
    test "" do
      peer_id = "abc123"

      peer = %{
        metadata: %{location: "San Francisco"},
        mid_to_track_metadata: %{"DEFCBA4321" => %{}}
      }

      assert {:sfu_media_event, :broadcast, data} =
               MediaEvent.create_peer_updated_event(peer_id, peer)

      assert Jason.decode!(data) == %{
               "data" => %{
                 "metadata" => %{"location" => "San Francisco"},
                 "midToTrackMetadata" => %{"DEFCBA4321" => %{}},
                 "peerId" => "abc123"
               },
               "type" => "peerUpdated"
             }
    end
  end

  describe "deserializing join media event" do
    test "creates proper map when event is valid" do
      raw_media_event =
        %{
          "type" => "join",
          "data" => %{
            "relayAudio" => true,
            "relayVideo" => true,
            "receiveMedia" => true,
            "metadata" => %{"displayName" => "Bob"},
            "tracksMetadata" => [
              %{"type" => "audio", "source" => "microphone"},
              %{"type" => "video", "source" => "camera"}
            ]
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :join,
        data: %{
          relay_audio: true,
          relay_video: true,
          receive_media: true,
          metadata: %{"displayName" => "Bob"},
          tracks_metadata: [
            %{"type" => "audio", "source" => "microphone"},
            %{"type" => "video", "source" => "camera"}
          ]
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.deserialize(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "join",
          "data" => %{
            "relayAudio" => true,
            # missing relayVideo field
            "receiveMedia" => true,
            "metadata" => %{"displayName" => "Bob"},
            "trackMetadata" => [
              %{"type" => "audio", "source" => "microphone"},
              %{"type" => "video", "source" => "camera"}
            ]
          }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.deserialize(raw_media_event)
    end
  end

  describe "deserializing updatePeerMetadata media event" do
    test "creates a media event with metadata when event is valid" do
      raw_media_event =
        %{
          "type" => "updatePeerMetadata",
          "data" => %{
            "metadata" => %{"displayName" => "Bob"},
            "midToTrackMetadata" => %{
              "ACBDEF" => %{"type" => "audio"},
              "FEDBCA" => %{"type" => "video"}
            }
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :update_peer_metadata,
        data: %{
          metadata: %{"displayName" => "Bob"}
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.deserialize(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "updatePeerMetadata",
          "data" => %{
            # missing metadata field
            "midToTrackMetadata" => %{
              "ACBDEF" => %{"type" => "audio"},
              "FEDBCA" => %{"type" => "video"}
            }
          }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.deserialize(raw_media_event)
    end
  end

  describe "deserializing updateTracksMetadata media event" do
    test "creates proper map when event is valid" do
      raw_media_event =
        %{
          "type" => "updateTracksMetadata",
          "data" => %{
            "midToTrackMetadata" => %{
              "ACBDEF" => %{"type" => "audio"},
              "FEDBCA" => %{"type" => "video"}
            }
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :update_tracks_metadata,
        data: %{
          mid_to_track_metadata: %{
            "ACBDEF" => %{"type" => "audio"},
            "FEDBCA" => %{"type" => "video"}
          }
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.deserialize(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "updateTracksMetadata",
          "data" => %{
            "random" => "data"
            # missing tracksMetadata field
          }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.deserialize(raw_media_event)
    end
  end
end
