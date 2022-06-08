defmodule Membrane.RTC.Engine.MediaEventTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.MediaEvent

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
end
