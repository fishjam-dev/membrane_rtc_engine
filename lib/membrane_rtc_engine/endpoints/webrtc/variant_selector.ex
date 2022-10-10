defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelector do
  @moduledoc false
  # module responsible for choosing track variant
  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionProber
  alias Membrane.RTC.Engine.Track

  @default_bitrates_video %{
    high: 1_500_000,
    medium: 500_000,
    low: 150_000,
    nil: 0
  }

  @default_bitrates_audio %{high: 50_000, nil: 0}

  @typep variant_t() :: Track.variant() | nil
  @typep bitrates_t() :: %{variant_t() => non_neg_integer()}

  @type t() :: %__MODULE__{
          target_variant: Track.variant(),
          current_variant: variant_t(),
          queued_variant: variant_t(),
          active_variants: MapSet.t(Track.variant()),
          current_allocation: non_neg_integer(),
          variant_bitrates: bitrates_t(),
          connection_prober: pid()
        }

  @enforce_keys [:current_allocation, :connection_prober, :variant_bitrates]
  defstruct @enforce_keys ++
              [
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
  @spec new(pid(), Track.t(), Track.variant()) :: t()
  def new(connection_prober, track, initial_target_variant \\ :high) do
    allocation =
      case track.type do
        :audio -> 60_000
        :video -> 200_000
      end

    ConnectionProber.say_hello(connection_prober, allocation, track)

    %__MODULE__{
      target_variant: initial_target_variant,
      connection_prober: connection_prober,
      variant_bitrates:
        case track.type do
          :audio -> @default_bitrates_audio
          :video -> @default_bitrates_video
        end,
      current_allocation: allocation
    }
  end

  @spec set_variant_bitrates(t(), bitrates_t()) :: {t(), variant_t()}
  def set_variant_bitrates(%__MODULE__{} = selector, bitrates) do
    bitrates = Map.new(bitrates, fn {k, v} -> {k, v.estimation} end)
    bitrates = Map.merge(selector.variant_bitrates, bitrates)

    selector
    |> Map.put(:variant_bitrates, bitrates)
    |> perform_automatic_layer_selection()
  end

  @spec set_bandwidth_allocation(t(), bitrates_t()) :: {t(), variant_t()}
  def set_bandwidth_allocation(%__MODULE__{} = selector, allocation) do
    selector
    |> Map.put(:current_allocation, allocation)
    |> perform_automatic_layer_selection()
  end

  @doc """
  Marks given `variant` as inactive.
  Returns new selector and variant to request
  or `nil` if there are no changes needed.
  """
  @spec variant_inactive(t(), Track.variant()) :: {t(), variant_t()}
  def variant_inactive(selector, variant) do
    Membrane.Logger.info("Variant #{variant} became inactive")

    selector = %__MODULE__{
      selector
      | active_variants: MapSet.delete(selector.active_variants, variant)
    }

    case selector do
      %{current_variant: ^variant} ->
        selector = %__MODULE__{selector | current_variant: nil}
        perform_automatic_layer_selection(selector)

      %{queued_variant: ^variant, current_variant: nil} ->
        selector = %__MODULE__{selector | queued_variant: nil}
        perform_automatic_layer_selection(selector)

      %{queued_variant: ^variant} ->
        selector = %__MODULE__{selector | queued_variant: nil}
        perform_automatic_layer_selection(selector)

      _else ->
        perform_automatic_layer_selection(selector)
    end
  end

  @doc """
  Marks given `variant` as active.
  Returns new selector and variant to request
  or `nil` if there are no changes needed.
  """
  @spec variant_active(t(), Track.variant()) :: {t(), variant_t()}
  def variant_active(selector, variant) do
    Membrane.Logger.info("Variant #{variant} became active")

    selector = %__MODULE__{
      selector
      | active_variants: MapSet.put(selector.active_variants, variant)
    }

    cond do
      # This clause makes sure that we're not doing anything if we already have target variant
      selector.target_variant in [selector.current_variant, selector.queued_variant] and
          selector.variant_bitrates[selector.target_variant] <= selector.current_allocation ->
        {selector, nil}
        |> tap(fn {selector, _variant} -> manage_allocation(selector) end)

      # This clause makes sure that we're selecting target variant if it becomes active and if we have enough bandwidth for it
      selector.target_variant == variant and
          selector.variant_bitrates[selector.target_variant] * 1.2 <= selector.current_allocation ->
        selector
        |> select_variant(variant)
        |> tap(fn {selector, _variant} -> manage_allocation(selector) end)

      # This clause runs automatic layer selection algorithm if we haven't yet seen the target of if we don't have enough bandwidth for the target
      true ->
        perform_automatic_layer_selection(selector)
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
  @spec set_target_variant(t(), Track.variant()) :: {t(), variant_t()}
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

  defp select_variant(%__MODULE__{} = selector) do
    variant = best_active_variant(selector)
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

  defp best_active_variant(selector) do
    selector.active_variants
    |> sort_variants()
    |> Enum.filter(
      &(Map.fetch!(selector.variant_bitrates, &1) * 1.1 <= selector.current_allocation)
    )
    |> List.first()
  end

  # TODO: refactor `next_desired_variant`
  # This function clause makes sure that we're no longer looking for a better variant when we already use target variant
  # NOTE: nil check is used because `nil` in target_variant means that we do not have any target variant
  defp next_desired_variant(%__MODULE__{target_variant: variant} = selector)
       when not is_nil(variant) and variant in [selector.current_variant, selector.queued_variant],
       do: {:error, :doesnt_exist}

  defp next_desired_variant(%__MODULE__{current_variant: variant} = selector) do
    selector.active_variants
    |> sort_variants()
    |> Enum.reverse()
    |> then(fn
      variants when is_nil(variant) -> [nil | variants]
      variants -> Enum.drop_while(variants, &(&1 != variant))
    end)
    |> case do
      [^variant, next | _variants] -> {:ok, next}
      _otherwise -> {:error, :doesnt_exist}
    end
  end

  defp next_desired_variant(_selector), do: {:error, :doesnt_exist}

  defp sort_variants(variants) do
    Enum.sort_by(
      variants,
      fn
        :high -> 3
        :medium -> 2
        :low -> 1
        nil -> 0
      end,
      :desc
    )
  end

  defp perform_automatic_layer_selection(%__MODULE__{} = selector) do
    selector
    |> select_variant()
    |> tap(fn {selector, _variant} -> manage_allocation(selector) end)
  end

  defp manage_allocation(%__MODULE__{} = selector) do
    current_variant_bitrate = selector.variant_bitrates[selector.current_variant]
    queued_variant_bitrate = selector.variant_bitrates[selector.queued_variant]

    required_bitrate =
      [current_variant_bitrate, queued_variant_bitrate]
      |> Enum.max()

    next_variant = next_desired_variant(selector)

    cond do
      # If we're not having that much margin left in current allocation, try to request higher allocation
      # It is very important that this clause takes precedence over the one that aims to increase quality.
      # If we don't have enough bandwidth to maintain current quality, don't bother with better quality, try to salvage current quality
      required_bitrate > 0.95 * selector.current_allocation ->
        Membrane.Logger.info(
          "Requesting #{required_bitrate / 1024} kbps from connection prober as a mean to maintain current quality"
        )

        ConnectionProber.request_allocation(selector.connection_prober, required_bitrate * 1.1)

      required_bitrate * 1.2 < selector.current_allocation ->
        Membrane.Logger.info(
          "Requesting #{required_bitrate / 1024} kbps from connection prober to free unused bitrate"
        )

        ConnectionProber.request_allocation(selector.connection_prober, required_bitrate * 1.1)

      # If there is a next variant that we want, let's try to request an allocation for it
      # We also don't want another
      match?({:ok, _variant}, next_variant) ->
        {:ok, variant} = next_variant
        bitrate = selector.variant_bitrates[variant] * 1.1

        Membrane.Logger.info(
          "Requesting #{bitrate / 1024} kbps from connection prober to increase quality"
        )

        ConnectionProber.request_allocation(
          selector.connection_prober,
          bitrate
        )

      true ->
        :ok
    end
  end
end