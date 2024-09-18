defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.VariantSelectorTest do
  use ExUnit.Case, async: true

  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.{NoOpConnectionAllocator, VariantSelector}

  @variant_bitrates %{
    high: 2_500_000,
    medium: 500_000,
    low: 150_000
  }

  test "VariantSelector selects another variant when currently used variant becomes inactive" do
    selector = create_selector()

    assert {selector, {:request, :medium, :variant_inactive}} =
             VariantSelector.variant_paused(selector, :high, :inactive)

    assert {_selector, {:request, :low, :variant_inactive}} =
             VariantSelector.variant_paused(selector, :medium, :inactive)
  end

  test "target_variant/2 sets target variant and chooses it when it is active" do
    selector = create_selector()

    assert {selector, {:request, :medium, :variant_inactive}} =
             VariantSelector.variant_paused(selector, :high, :inactive)

    assert {selector, :noop} = VariantSelector.set_target_variant(selector, :medium)

    selector = VariantSelector.set_current_variant(selector, :medium)

    # assert that variant selector doesn't request the new variant
    # even though it is better
    assert {_selector, :noop} = VariantSelector.variant_resumed(selector, :high)
  end

  test "target_variant/2 sets target variant but doesn't switch to it when it is inactive" do
    selector = create_selector()

    assert {selector, :noop} = VariantSelector.variant_paused(selector, :low, :inactive)
    assert {selector, :noop} = VariantSelector.set_target_variant(selector, :low)

    assert {_selector, {:request, :low, :variant_resumed}} =
             VariantSelector.variant_resumed(selector, :low)
  end

  test "VariantSelector doesn't select a new variant when not currently used variant is marked as inactive" do
    selector = create_selector()

    assert {selector, :noop} = VariantSelector.variant_paused(selector, :medium, :inactive)
    assert {_selector, :noop} = VariantSelector.variant_paused(selector, :low, :inactive)
  end

  describe "Muting Track" do
    test "VariantSelector marks as muted the last active variant" do
      selector = create_selector()
      assert selector.muted_variant == :no_variant

      selector = mute_all_variants(selector)
      assert selector.muted_variant == :high
      assert Enum.empty?(selector.active_variants)
    end

    test "VariantSelector selects another variant if the muted variant isn't active yet" do
      selector =
        create_selector()
        |> mute_all_variants()

      {selector, {:request, :medium, :automatic_selection}} =
        VariantSelector.variant_resumed(selector, :medium)

      assert selector.muted_variant == :high
      assert selector.active_variants == MapSet.new([:medium])
    end

    test "VariantSelector selects muted variant once it becomes active again" do
      selector =
        create_selector()
        |> mute_all_variants()

      {selector, {:request, :high, :variant_unmuted}} =
        VariantSelector.variant_resumed(selector, :high)

      assert selector.muted_variant == :no_variant
      assert selector.active_variants == MapSet.new([:high])
    end
  end

  describe "Automatic variant selection in VariantSelector" do
    setup do
      [
        selector:
          VariantSelector.new(
            %{type: :video, variants: [:low, :medium, :high]},
            Membrane.RTC.Engine.Endpoint.ExWebRTC.TestConnectionAllocator,
            self(),
            initial_target_variant: :high
          )
      ]
    end

    test "accepts any variant before receiving first allocation", %{selector: selector} do
      Enum.each([:low, :medium], fn variant ->
        assert {_selector, {:request, ^variant, :automatic_selection}} =
                 VariantSelector.variant_resumed(selector, variant)
      end)

      assert {_selector, {:request, :high, :variant_resumed}} =
               VariantSelector.variant_resumed(selector, :high)
    end

    test "frees unused allocation", %{selector: selector} do
      assert {_selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert_allocation_requested(:low)
    end

    test "requests allocation for higher variant", %{selector: selector} do
      assert {selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert_allocation_requested(:low)
      assert {_selector, :noop} = VariantSelector.variant_resumed(selector, :medium)
      assert_allocation_requested(:medium)
    end

    test "doesn't select a higher variant right after lowering its allocation, before receiving confirmation from the Allocator",
         %{selector: selector} do
      assert {selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert_allocation_requested(:low)
      assert {_selector, :noop} = VariantSelector.variant_resumed(selector, :medium)
    end

    test "selects the highest possible variant", %{selector: selector} do
      variants = [:low, :medium, :high]

      # activate all variants
      selector =
        Enum.reduce(variants, selector, fn variant, selector ->
          assert {selector, _action} = VariantSelector.variant_resumed(selector, variant)
          selector
        end)

      # ensuring correct initial conditions - selector now has the allocation only for low
      assert selector.queued_variant == :low
      assert_allocation_requested(:low)

      # slowly increase the allocation
      Enum.reduce([:medium, :high], selector, fn variant, selector ->
        allocation = @variant_bitrates[variant] * 1.1
        assert_allocation_requested(variant)

        assert {selector, {:request, ^variant, :set_bandwidth_allocation}} =
                 VariantSelector.set_bandwidth_allocation(selector, allocation)

        selector
      end)
    end

    test "requests to stop the track when allocation is forcefully lowered", %{selector: selector} do
      assert {selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert {_selector, :stop} = VariantSelector.set_bandwidth_allocation(selector, 0)
    end

    test "refuses to lower the allocation before receiving the first variant", %{
      selector: selector
    } do
      assert {_selector, :noop} = VariantSelector.decrease_allocation(selector)
      assert_receive {_pid, {:decrease_allocation_request, :reject}}
    end

    test "refuses to lower the allocation when on low", %{selector: selector} do
      assert {selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert {_selector, :noop} = VariantSelector.decrease_allocation(selector)
      assert_receive {_pid, {:decrease_allocation_request, :reject}}
    end

    test "lowers the allocation when on medium and it is requested", %{selector: selector} do
      assert {selector, {:request, :low, :automatic_selection}} =
               VariantSelector.variant_resumed(selector, :low)

      assert_allocation_requested(:low)
      assert {selector, :noop} = VariantSelector.variant_resumed(selector, :medium)
      assert_allocation_requested(:medium)

      assert {selector, {:request, :medium, :set_bandwidth_allocation}} =
               VariantSelector.set_bandwidth_allocation(
                 selector,
                 @variant_bitrates[:medium] * 1.1
               )

      selector = VariantSelector.set_current_variant(selector, :medium)

      assert {selector, {:request, :low, :low_bandwidth}} =
               VariantSelector.decrease_allocation(selector)

      assert_receive {_pid, {:decrease_allocation_request, :accept}}
      VariantSelector.set_current_variant(selector, :low)
      assert_allocation_requested(:low)
    end

    test "updates selector variant bitrates", %{selector: selector} do
      new_high_bitrate_kbps = 1999

      new_selector =
        VariantSelector.update_variant_bitrate(selector, :high, new_high_bitrate_kbps)

      high_bitrate = Map.get(new_selector.variant_bitrates, :high)

      assert high_bitrate == new_high_bitrate_kbps
    end
  end

  defp create_selector() do
    selector =
      VariantSelector.new(
        %{type: :video, variants: [:low, :medium, :high]},
        NoOpConnectionAllocator,
        self(),
        initial_target_variant: :high
      )

    selector = %{selector | current_allocation: 9_999_999_999_999}

    assert {selector, {:request, :high, :variant_resumed}} =
             VariantSelector.variant_resumed(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :high)

    assert {selector, :noop} = VariantSelector.variant_resumed(selector, :medium)
    assert {selector, :noop} = VariantSelector.variant_resumed(selector, :low)
    selector
  end

  defp assert_allocation_requested(variant) do
    bitrate = @variant_bitrates[variant] * 1.1
    assert_receive {:request_allocation, _pid, ^bitrate}
  end

  defp mute_all_variants(selector) do
    Enum.reduce([:low, :medium, :high], selector, fn variant, selector ->
      {selector, :noop} = VariantSelector.variant_paused(selector, variant, :muted)
      selector
    end)
  end
end
