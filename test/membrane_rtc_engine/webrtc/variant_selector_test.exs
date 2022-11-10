defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelectorTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{NoOpConnectionAllocator, VariantSelector}

  @variant_bitrates %{
    high: 1_500_000,
    medium: 500_000,
    low: 150_000
  }

  test "VariantSelector selects another variant when currently used variant becomes inactive" do
    selector = create_selector()
    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)
    assert {_selector, {:request, :low}} = VariantSelector.variant_inactive(selector, :medium)
  end

  # Skip explanation:
  # After introducing automatic variant switching, this feature seems to
  # be a out of date. This behavior will be either achieved
  # or overwritten by the desire to always send the highest possible or target
  # variant, that fits the bandwidth allocation
  @tag :skip
  test "VariantSelector selects variant being used before it became inactive" do
    selector = create_selector()

    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :medium)

    assert {_selector, {:request, :high}} = VariantSelector.variant_active(selector, :high)
  end

  test "target_variant/2 sets target variant and chooses it when it is active" do
    selector = create_selector()

    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)
    assert {selector, :noop} = VariantSelector.set_target_variant(selector, :medium)

    selector = VariantSelector.set_current_variant(selector, :medium)

    # assert that variant selector doesn't request the new variant
    # even though it is better
    assert {_selector, :noop} = VariantSelector.variant_active(selector, :high)
  end

  test "target_variant/2 sets target variant but doesn't switch to it when it is inactive" do
    selector = create_selector()

    assert {selector, :noop} = VariantSelector.variant_inactive(selector, :low)
    assert {selector, :noop} = VariantSelector.set_target_variant(selector, :low)
    assert {_selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
  end

  @tag :skip
  test "VariantSelector doesn't select a new variant when not currently used variant is marked as inactive" do
    selector = create_selector()

    assert {selector, :noop} = VariantSelector.variant_inactive(selector, :medium)
    assert {_selector, :noop} = VariantSelector.variant_inactive(selector, :low)
  end

  describe "Automatic variant selection in VariantSelector" do
    setup do
      [
        selector:
          VariantSelector.new(
            %{type: :video},
            Membrane.RTC.Engine.Endpoint.WebRTC.TestConnectionAllocator,
            self(),
            :high
          )
      ]
    end

    test "accepts any variant before receiving first allocation", %{selector: selector} do
      Enum.each([:low, :medium, :high], fn variant ->
        assert {_selector, {:request, ^variant}} =
                 VariantSelector.variant_active(selector, variant)
      end)
    end

    test "frees unused allocation", %{selector: selector} do
      assert {_selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert_allocation_requested(:low)
    end

    test "requests allocation for higher variant", %{selector: selector} do
      assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert_allocation_requested(:low)
      assert {_selector, :noop} = VariantSelector.variant_active(selector, :medium)
      assert_allocation_requested(:medium)
    end

    test "doesn't select a higher variant right after lowering its allocation, before receiving confirmation from the Allocator",
         %{selector: selector} do
      assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert_allocation_requested(:low)
      assert {_selector, :noop} = VariantSelector.variant_active(selector, :medium)
    end

    test "selects the highest possible variant", %{selector: selector} do
      variants = [:low, :medium, :high]

      # activate all variants
      selector =
        Enum.reduce(variants, selector, fn variant, selector ->
          assert {selector, _action} = VariantSelector.variant_active(selector, variant)
          selector
        end)

      # ensuring correct initial conditions - selector now has the allocation only for low
      assert selector.queued_variant == :low
      assert_allocation_requested(:low)

      # slowly increase the allocation
      Enum.reduce([:medium, :high], selector, fn variant, selector ->
        allocation = @variant_bitrates[variant] * 1.1
        assert_allocation_requested(variant)

        assert {selector, {:request, ^variant}} =
                 VariantSelector.set_bandwidth_allocation(selector, allocation)

        selector
      end)
    end

    test "requests to stop the track when allocation is forcefully lowered", %{selector: selector} do
      assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert {_selector, :stop} = VariantSelector.set_bandwidth_allocation(selector, 0)
    end

    test "refuses to lower the allocation before receiving the first variant", %{
      selector: selector
    } do
      assert {_selector, :noop} = VariantSelector.decrease_allocation(selector)
      assert_receive {_pid, {:decrease_allocation_request, :reject}}
    end

    test "refuses to lower the allocation when on low", %{selector: selector} do
      assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert {_selector, :noop} = VariantSelector.decrease_allocation(selector)
      assert_receive {_pid, {:decrease_allocation_request, :reject}}
    end

    test "lowers the allocation when on medium and it is requested", %{selector: selector} do
      assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
      assert_allocation_requested(:low)
      assert {selector, :noop} = VariantSelector.variant_active(selector, :medium)
      assert_allocation_requested(:medium)

      assert {selector, {:request, :medium}} =
               VariantSelector.set_bandwidth_allocation(
                 selector,
                 @variant_bitrates[:medium] * 1.1
               )

      selector = VariantSelector.set_current_variant(selector, :medium)

      assert {selector, {:request, :low}} = VariantSelector.decrease_allocation(selector)
      assert_receive {_pid, {:decrease_allocation_request, :accept}}
      VariantSelector.set_current_variant(selector, :low)
      assert_allocation_requested(:low)
    end
  end

  defp create_selector() do
    selector = VariantSelector.new(%{type: :video}, NoOpConnectionAllocator, self(), :high)
    selector = %{selector | current_allocation: 9_999_999_999_999}

    assert {selector, {:request, :high}} = VariantSelector.variant_active(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :high)

    assert {selector, :noop} = VariantSelector.variant_active(selector, :medium)
    assert {selector, :noop} = VariantSelector.variant_active(selector, :low)
    selector
  end

  defp assert_allocation_requested(variant) do
    bitrate = @variant_bitrates[variant] * 1.1
    assert_receive {:request_allocation, _pid, ^bitrate}
  end
end
