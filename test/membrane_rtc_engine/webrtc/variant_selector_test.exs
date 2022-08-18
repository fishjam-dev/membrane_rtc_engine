defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelectorTest do
  use ExUnit.Case

  alias Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelector

  test "VariantSelector selects another variant when currently used variant becomes inactive" do
    selector = create_selector()
    assert {selector, :medium} = VariantSelector.variant_inactive(selector, :high)
    assert {_selector, :low} = VariantSelector.variant_inactive(selector, :medium)
  end

  test "VariantSelector selects variant being used before it became inactive" do
    selector = create_selector()

    assert {selector, :medium} = VariantSelector.variant_inactive(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :medium)

    assert {_selector, :high} = VariantSelector.variant_active(selector, :high)
  end

  test "target_variant/2 sets target variant and chooses it when it is active" do
    selector = create_selector()

    assert {selector, :medium} = VariantSelector.variant_inactive(selector, :high)
    assert {selector, :medium} = VariantSelector.set_target_variant(selector, :medium)

    selector = VariantSelector.set_current_variant(selector, :medium)

    # assert that variant selector doesn't request the new variant
    # even though it is better
    assert {_selector, nil} = VariantSelector.variant_active(selector, :high)
  end

  test "target_variant/2 sets target variant but doesn't switch to it when it is inactive" do
    selector = create_selector()

    assert {selector, nil} = VariantSelector.variant_inactive(selector, :low)
    assert {selector, nil} = VariantSelector.set_target_variant(selector, :low)
    assert {_selector, :low} = VariantSelector.variant_active(selector, :low)
  end

  test "target_variant/2 ignores non-existing variant" do
    selector = create_selector()

    non_existing_variant = "non-existing-variant"

    assert {selector, nil} = VariantSelector.set_target_variant(selector, non_existing_variant)
    assert selector.target_variant != non_existing_variant
  end

  test "VariantSelector doesn't select a new variant when not currently used variant is marked as inactive" do
    selector = create_selector()

    assert {selector, nil} = VariantSelector.variant_inactive(selector, :medium)
    assert {_selector, nil} = VariantSelector.variant_inactive(selector, :low)
  end

  test "VariantSelector selects a new variant when it is better than currently used variant and while waiting for the target variant" do
    selector = VariantSelector.new([:high, :medium, :low], :high)

    assert {selector, :low} = VariantSelector.variant_active(selector, :low)
    assert {selector, :medium} = VariantSelector.variant_active(selector, :medium)
    assert {_selector, :high} = VariantSelector.variant_active(selector, :high)
  end

  defp create_selector() do
    selector = VariantSelector.new([:high, :medium, :low], :high)

    assert {selector, :high} = VariantSelector.variant_active(selector, :high)

    selector = VariantSelector.set_current_variant(selector, :high)

    assert {selector, nil} = VariantSelector.variant_active(selector, :medium)
    assert {selector, nil} = VariantSelector.variant_active(selector, :low)
    selector
  end
end
