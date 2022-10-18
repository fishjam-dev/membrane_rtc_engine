defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelector do
  @moduledoc false
  # module responsible for choosing track variant
  require Membrane.Logger

  alias Membrane.RTC.Engine.Track

  @default_bitrates_video %{
    high: 1_500_000,
    medium: 500_000,
    low: 150_000,
    no_variant: 0
  }

  @default_bitrates_audio %{high: 50_000, no_variant: 0}

  @typep variant_t() :: Track.variant() | :no_variant
  @typep bitrates_t() :: %{variant_t() => non_neg_integer()}

  @type t() :: %__MODULE__{
          target_variant: Track.variant() | nil,
          current_variant: variant_t(),
          queued_variant: variant_t(),
          active_variants: MapSet.t(Track.variant()),
          current_allocation: non_neg_integer(),
          variant_bitrates: bitrates_t(),
          connection_allocator: pid(),
          connection_allocator_module: module()
        }

  @enforce_keys [
    :current_allocation,
    :connection_allocator,
    :connection_allocator_module,
    :variant_bitrates
  ]
  defstruct @enforce_keys ++
              [
                :target_variant,
                :queued_variant,
                current_variant: :no_variant,
                active_variants: MapSet.new()
              ]

  @doc """
  Creates new variant selector.

  * `initial_target_variant` - variant to prioritize. It will be
  chosen whenever it is active. Can be changed with `set_target_variant/2`.
  """
  @spec new(Track.t(), module(), pid(), Track.variant()) :: t()
  def new(
        track,
        connection_allocator_module,
        connection_allocator,
        initial_target_variant \\ :high
      ) do
    variant_bitrates =
      case track.type do
        :audio -> @default_bitrates_audio
        :video -> @default_bitrates_video
      end

    initial_allocation =
      case track.type do
        :audio -> variant_bitrates[:high]
        :video -> variant_bitrates[:low]
      end

    connection_allocator_module.say_hello(connection_allocator, initial_allocation, track)

    %__MODULE__{
      target_variant: initial_target_variant,
      connection_allocator: connection_allocator,
      variant_bitrates: variant_bitrates,
      current_allocation: initial_allocation,
      connection_allocator_module: connection_allocator_module
    }
  end

  @doc """
  Updates the bandwidth allocated to the selector.

  Returns new selector and variant to request,
  nil if there are no changes needed
  or `:stop` if you should stop receiving the track in question
  """
  @spec set_bandwidth_allocation(t(), bitrates_t()) :: {t(), Track.variant() | :stop | nil}
  def set_bandwidth_allocation(%__MODULE__{} = selector, allocation) do
    selector
    |> Map.put(:current_allocation, allocation)
    |> perform_automatic_layer_selection()
  end

  @doc """
  Marks given `variant` as inactive.

  Returns new selector and variant to request,
  `nil` if there are no changes needed
  or `:stop` if you should stop receiving the track in question
  """
  @spec variant_inactive(t(), Track.variant()) :: {t(), Track.variant() | :stop | nil}
  def variant_inactive(selector, variant) do
    selector = %__MODULE__{
      selector
      | active_variants: MapSet.delete(selector.active_variants, variant)
    }

    case selector do
      %{current_variant: ^variant} ->
        %__MODULE__{selector | current_variant: :no_variant}

      %{queued_variant: ^variant, current_variant: :no_variant} ->
        %__MODULE__{selector | queued_variant: nil}

      %{queued_variant: ^variant} ->
        %__MODULE__{selector | queued_variant: nil}

      _else ->
        selector
    end
    |> perform_automatic_layer_selection()
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
      not is_nil(selector.target_variant) and
          selector.target_variant in [selector.current_variant, selector.queued_variant] ->
        {selector, nil}

      # make sure we're selecting a target variant if it becomes active and we have the bandwidth
      selector.target_variant == variant and can_switch_to_variant(selector, variant) ->
        selector
        |> select_variant(variant)
        |> tap(&manage_allocation/1)

      # otherwise, we're waiting for target, so automatically select a variant in the meantime
      true ->
        selector
        |> perform_automatic_layer_selection()
        |> case do
          {selector, :stop} -> {selector, nil}
          {selector, action} -> {selector, action}
        end
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
    |> tap(&manage_allocation/1)
  end

  def set_current_variant(%__MODULE__{} = selector, variant) do
    %__MODULE__{selector | current_variant: variant}
    |> tap(&manage_allocation/1)
  end

  @doc """
  Sets the target variant that should be selected whenever it is active.

  If the target variant is not active, we will switch to it when it becomes active again
  """
  @spec set_target_variant(t(), Track.variant()) :: {t(), Track.variant() | nil}
  def set_target_variant(selector, variant) do
    if variant in selector.active_variants and can_switch_to_variant(selector, variant) do
      selector = %__MODULE__{selector | target_variant: variant}

      selector
      |> select_variant(variant)
      |> tap(&manage_allocation/1)
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

  defp select_variant(selector, :no_variant) do
    Membrane.Logger.debug("No active variant.")
    selector = %__MODULE__{selector | current_variant: :no_variant, queued_variant: nil}
    {selector, :stop}
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
    |> MapSet.put(:no_variant)
    |> sort_variants()
    |> Enum.filter(&can_switch_to_variant(selector, &1))
    |> List.first()
  end

  # This function clause makes sure that we're no longer looking for a better variant when we already use target variant
  defp next_desired_variant(%__MODULE__{target_variant: target_variant} = selector)
       when target_variant in [selector.current_variant, selector.queued_variant],
       do: {:error, :doesnt_exist}

  defp next_desired_variant(
         %__MODULE__{current_variant: current_variant, queued_variant: queued_variant} = selector
       ) do
    pivot = queued_variant || current_variant

    MapSet.put(selector.active_variants, :no_variant)
    |> sort_variants()
    |> Enum.reverse()
    |> Enum.drop_while(&(&1 != pivot))
    |> case do
      [^pivot] -> {:error, :doesnt_exist}
      [^pivot | other] -> {:ok, List.first(other)}
    end
  end

  defp sort_variants(variants) do
    Enum.sort_by(
      variants,
      fn
        :high -> 3
        :medium -> 2
        :low -> 1
        :no_variant -> 0
      end,
      :desc
    )
  end

  defp perform_automatic_layer_selection(%__MODULE__{} = selector) do
    selector
    |> select_variant()
    |> tap(&manage_allocation/1)
  end

  defp manage_allocation({selector, _variant}), do: manage_allocation(selector)

  defp manage_allocation(%__MODULE__{} = selector) do
    current_variant_bitrate = selector.variant_bitrates[selector.current_variant]
    queued_variant_bitrate = selector.variant_bitrates[selector.queued_variant] || 0

    required_bitrate =
      [current_variant_bitrate, queued_variant_bitrate]
      |> Enum.max()

    next_variant = next_desired_variant(selector)

    cond do
      # If we're not having that much margin left in current allocation, try to request higher allocation
      # It is very important that this clause takes precedence over the one that aims to increase quality.
      # If we don't have enough bandwidth to maintain current quality, don't bother with better quality, try to salvage current quality
      required_bitrate > 0.95 * selector.current_allocation ->
        Membrane.Logger.debug(
          "Requesting #{required_bitrate / 1024} kbps from connection prober as a mean to maintain current quality"
        )

        selector.connection_allocator_module.request_allocation(
          selector.connection_allocator,
          required_bitrate * 1.1
        )

      required_bitrate * 1.2 < selector.current_allocation ->
        Membrane.Logger.debug(
          "Requesting #{required_bitrate / 1024} kbps from connection prober to free unused bitrate"
        )

        selector.connection_allocator_module.request_allocation(
          selector.connection_allocator,
          required_bitrate * 1.1
        )

      # If there is a next variant that we want, let's try to request an allocation for it
      # We also don't want another
      match?({:ok, _variant}, next_variant) ->
        {:ok, variant} = next_variant
        bitrate = selector.variant_bitrates[variant] * 1.1

        Membrane.Logger.debug(
          "Requesting #{bitrate / 1024} kbps from connection prober to increase quality"
        )

        selector.connection_allocator_module.request_allocation(
          selector.connection_allocator,
          bitrate
        )

      true ->
        :ok
    end
  end

  defp can_switch_to_variant(selector, variant) do
    selector.variant_bitrates[variant] * 1.1 <= selector.current_allocation
  end
end
