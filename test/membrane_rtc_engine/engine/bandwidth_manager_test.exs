defmodule Membrane.RTC.Engine.BandwidthManagerTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.BandwidthManager

  @subscriptions %{
    "endpoint1" => %{
      "track1" => nil,
      "track2" => nil
    }
  }

  @all_layers ["h", "m", "l"]

  test "Returns all layers when no estimations are present" do
    manager = BandwidthManager.new()

    manager
    |> BandwidthManager.generate_allowed_layers(@subscriptions)
    |> Enum.each(fn {_key, layers} ->
      assert layers |> MapSet.new() |> MapSet.equal?(MapSet.new(@all_layers))
    end)
  end

  test "successfully drops 'h' layer when there isn't enough bandwidth" do
    manager =
      BandwidthManager.new()
      |> BandwidthManager.add_endpoint_estimation("endpoint1", 1_600)
      |> BandwidthManager.add_track_estimation("track1", %{"h" => 1200, "m" => 800, "l" => 200})
      |> BandwidthManager.add_track_estimation("track2", %{"h" => 1200, "m" => 800, "l" => 200})

    layers = BandwidthManager.generate_allowed_layers(manager, @subscriptions)

    layers
    |> Map.values()
    |> Enum.each(&assert @all_layers -- &1 == ["h"])
  end
end
