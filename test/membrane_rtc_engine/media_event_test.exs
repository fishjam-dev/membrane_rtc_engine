defmodule Membrane.RTC.Engine.MediaEventTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.MediaEvent

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
end
