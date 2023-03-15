defmodule Membrane.RTC.Engine.Endpoint.WebRTC.MediaEventTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.MediaEvent

  describe "deserializing join media event" do
    test "creates proper map when event is valid" do
      raw_media_event =
        %{
          "type" => "join",
          "data" => %{
            "receiveMedia" => true,
            "metadata" => %{"displayName" => "Bob"}
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :join,
        data: %{
          metadata: %{"displayName" => "Bob"}
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.decode(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "join",
          "data" =>
            %{
              # missing metadata field
            }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.decode(raw_media_event)
    end
  end

  describe "deserializing sdpOffer media event" do
    test "creates proper map when event is valid" do
      metadata = %{"track_id" => %{"abc" => "cba"}}
      bitrates = %{"track_id" => %{"l" => 100, "m" => 200, "h" => 300}}
      mids = %{"5" => "track_id"}
      sdp = "mock_sdp"

      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "sdpOffer",
            "data" => %{
              "sdpOffer" => %{
                "type" => "offer",
                "sdp" => sdp
              },
              "trackIdToTrackMetadata" => metadata,
              "trackIdToTrackBitrates" => bitrates,
              "midToTrackId" => mids
            }
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :custom,
        data: %{
          type: :sdp_offer,
          data: %{
            sdp_offer: %{
              type: :offer,
              sdp: sdp
            },
            track_id_to_track_metadata: metadata,
            track_id_to_track_bitrates: bitrates,
            mid_to_track_id: mids
          }
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.decode(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "sdpOffer",
            "data" => %{
              "sdpOffer" => %{
                "type" => "offer",
                "sdp" => "mock_sdp"
              }
            }
          }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.decode(raw_media_event)
    end
  end

  describe "deserializing trackVariantBitrates media event" do
    test "creates proper map when event is valid" do
      track_id = "track_id"
      bitrates = %{"h" => 1000, "m" => 500}

      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "trackVariantBitrates",
            "data" => %{
              "trackId" => track_id,
              "variantBitrates" => bitrates
            }
          }
        }
        |> Jason.encode!()

      expected_media_event = %{
        type: :custom,
        data: %{
          type: :track_variant_bitrates,
          data: %{
            track_id: track_id,
            variant_bitrates: bitrates
          }
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.decode(raw_media_event)
    end

    test "returns error when event misses key" do
      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "trackVariantBitrates",
            "data" => %{
              "trackId" => "track_id"
              # missing variantBitrates
            }
          }
        }
        |> Jason.encode!()

      assert {:error, :invalid_media_event} == MediaEvent.decode(raw_media_event)
    end
  end
end
