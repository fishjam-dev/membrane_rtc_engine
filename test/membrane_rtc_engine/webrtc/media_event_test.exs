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
    setup do
      %{
        sdp: "mock_sdp",
        track_id: "mock_track_id",
        track_metadata: %{"abc" => "cba"},
        max_bandwidth: %{"l" => 100, "m" => 200, "h" => 300},
        mid: 5
      }
    end

    test "creates proper map when event is valid", context do
      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "sdpOffer",
            "data" => %{
              "sdpOffer" => %{
                "type" => "offer",
                "sdp" => context.sdp
              },
              "trackIdToTrackInfo" => %{
                context.track_id => %{
                  "trackMetadata" => context.track_metadata,
                  "maxBandwidth" => context.max_bandwidth,
                  "mid" => context.mid
                }
              }
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
              sdp: context.sdp
            },
            track_id_to_track_info: %{
              context.track_id => %{
                track_metadata: context.track_metadata,
                max_bandwidth: context.max_bandwidth,
                mid: context.mid
              }
            }
          }
        }
      }

      assert {:ok, expected_media_event} == MediaEvent.decode(raw_media_event)
    end

    test "returns error when event misses key", context do
      raw_media_event =
        %{
          "type" => "custom",
          "data" => %{
            "type" => "sdpOffer",
            "data" => %{
              "sdpOffer" => %{
                "type" => "offer",
                "sdp" => context.sdp
              },
              "trackIdToTrackInfo" => %{
                context.track_id => %{
                  "trackMetadata" => context.track_metadata,
                  # missing maxBandwidth key
                  "mid" => context.mid
                }
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
      track_id = "mock_track_id"
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
  end
end
