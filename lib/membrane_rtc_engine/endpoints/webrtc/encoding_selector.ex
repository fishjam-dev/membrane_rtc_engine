defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelector do
  @moduledoc false
  # module responsible for choosing track variant

  require Membrane.Logger

  alias Membrane.RTC.Engine.Track

  @type t() :: %__MODULE__{
          target_variant: Track.variant(),
          current_variant: Track.variant() | nil,
          queued_variant: Track.variant() | nil,
          active_variants: MapSet.t(Track.variant())
        }

  defstruct [
    :target_variant,
    :current_variant,
    :queued_variant,
    active_variants: MapSet.new()
  ]

  @doc """
  Creates new variant selector.

  * `initial_target_variant` - variant to prioritize. It will be
  chosen whenever it is active. Can be changed with `set_target_variant/2`.
  """
  @spec new(Track.variant()) :: t()
  def new(initial_target_variant \\ :high) do
    %__MODULE__{target_variant: initial_target_variant}
  end

  @doc """
  Marks given `variant` as inactive.

  Returns new selector and variant to request
  or `nil` if there are no changes needed.
  """
  @spec variant_inactive(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def variant_inactive(selector, variant) do
    selector = %__MODULE__{
      selector
      | active_variants: MapSet.delete(selector.active_variants, variant)
    }

    case selector do
      %{current_variant: ^variant} ->
        selector = %__MODULE__{selector | current_variant: nil}
        select_variant(selector)

      %{queued_variant: ^variant, current_variant: nil} ->
        selector = %__MODULE__{selector | queued_variant: nil}
        select_variant(selector)

      %{queued_variant: ^variant} ->
        selector = %__MODULE__{selector | queued_variant: nil}
        {selector, nil}

      _else ->
        {selector, nil}
    end
  end

  @doc """
  Marks given `variant` as active.

  Returns new selector and variant to request
  or `nil` if there are no changes needed.
  """
  @spec variant_active(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def variant_active(selector, variant) do
    selector = %__MODULE__{
      selector
      | active_variants: MapSet.put(selector.active_variants, variant)
    }

    cond do
      selector.current_variant == selector.target_variant ->
        {selector, nil}

      selector.queued_variant == selector.target_variant ->
        {selector, nil}

      selector.target_variant == variant ->
        select_variant(selector, variant)

      true ->
        select_variant(selector)
    end
  end

  @doc """
  Sets currently used variant.

  Should be called when variant change happens
  i.e. after receiving `Membrane.RTC.Engine.Event.TrackVariantSwitched` event.
  """
  @spec set_current_variant(t(), Track.variant()) :: t()
  def set_current_variant(%__MODULE__{queued_variant: variant} = selector, variant) do
    %__MODULE__{selector | current_variant: variant, queued_variant: nil}
  end

  def set_current_variant(%__MODULE__{} = selector, variant) do
    %__MODULE__{selector | current_variant: variant}
  end

  @doc """
  Sets the target variant that should be selected whenever it is active.

  If the target variant is not active, we will switch to it when it becomes active again
  """
  @spec set_target_variant(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def set_target_variant(selector, variant) do
    if variant in selector.active_variants do
      selector = %__MODULE__{selector | target_variant: variant}
      select_variant(selector, variant)
    else
      Membrane.Logger.debug("""
      Requested inactive variant #{inspect(variant)}. Saving it as target.
      It will be requested once it becomes active
      """)

      selector = %__MODULE__{selector | target_variant: variant}

      {selector, nil}
    end
  end

  defp select_variant(%__MODULE__{active_variants: variants} = selector) do
    variant = best_active_variant(variants)
    select_variant(selector, variant)
  end

  defp select_variant(selector, nil) do
    Membrane.Logger.debug("No active variant.")
    selector = %__MODULE__{selector | current_variant: nil, queued_variant: nil}
    {selector, nil}
  end

  defp select_variant(
         %__MODULE__{current_variant: variant, queued_variant: nil} = selector,
         variant
       ) do
    Membrane.Logger.debug("Requested currently used variant #{variant}. Ignoring.")
    {selector, nil}
  end

  defp select_variant(%__MODULE__{current_variant: variant} = selector, variant) do
    Membrane.Logger.debug("""
    Requested variant: #{inspect(variant)} which is currently used but while waiting
    for keyframe for queued_variant #{inspect(selector.queued_variant)}.
    Clearing queued_variant #{inspect(selector.queued_variant)}
    """)

    selector = %__MODULE__{selector | queued_variant: nil}
    {selector, nil}
  end

  defp select_variant(selector, variant) do
    Membrane.Logger.debug("Enqueuing variant #{inspect(variant)}.")
    selector = %__MODULE__{selector | queued_variant: variant}
    {selector, variant}
  end

  defp best_active_variant(variants) do
    variants |> sort_variants() |> List.first()
  end

  defp sort_variants(variants) do
    Enum.sort_by(
      variants,
      fn
        :high -> 3
        :medium -> 2
        :low -> 1
      end,
      :desc
    )
  end
end
