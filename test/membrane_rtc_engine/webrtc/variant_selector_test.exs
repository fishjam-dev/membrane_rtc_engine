defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelectorTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{DefaultConnectionAllocator, VariantSelector}

  test "VariantSelector selects another variant when currently used variant becomes inactive" do
    selector = create_selector()
    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)
    assert {_selector, {:request, :low}} = VariantSelector.variant_inactive(selector, :medium)
  end

  test "VariantSelector selects variant being used before it became inactive" do
    selector = create_selector()

    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :medium)

    assert {_selector, {:request, :high}} = VariantSelector.variant_active(selector, :high)
  end

  test "target_variant/2 sets target variant and chooses it when it is active" do
    selector = create_selector()

    assert {selector, {:request, :medium}} = VariantSelector.variant_inactive(selector, :high)
    assert {selector, {:request, :medium}} = VariantSelector.set_target_variant(selector, :medium)

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

  test "VariantSelector doesn't select a new variant when not currently used variant is marked as inactive" do
    selector = create_selector()

    assert {selector, :noop} = VariantSelector.variant_inactive(selector, :medium)
    assert {_selector, :noop} = VariantSelector.variant_inactive(selector, :low)
  end

  test "VariantSelector selects a new variant when it is better than currently used variant and while waiting for the target variant" do
    selector = VariantSelector.new(%{type: :video}, DefaultConnectionAllocator, self(), :high)
    selector = %{selector | current_allocation: 9_999_999_999_999}

    assert {selector, {:request, :low}} = VariantSelector.variant_active(selector, :low)
    assert {selector, {:request, :medium}} = VariantSelector.variant_active(selector, :medium)
    assert {_selector, {:request, :high}} = VariantSelector.variant_active(selector, :high)
  end

  defp create_selector() do
    selector = VariantSelector.new(%{type: :video}, DefaultConnectionAllocator, self(), :high)
    selector = %{selector | current_allocation: 9_999_999_999_999}

    assert {selector, {:request, :high}} = VariantSelector.variant_active(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :high)

    assert {selector, :noop} = VariantSelector.variant_active(selector, :medium)
    assert {selector, :noop} = VariantSelector.variant_active(selector, :low)
    selector
  end
end
