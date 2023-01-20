defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantSelector do
  @moduledoc false
  # module responsible for choosing track variant
  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Track

  @default_bitrates_video %{
    high: 1_500_000,
    medium: 500_000,
    low: 150_000,
    no_variant: 0
  }

  @default_bitrates_audio %{high: 50_000, no_variant: 0}

  @typep bitrates_t() :: %{(Track.variant() | :no_variant) => non_neg_integer()}

  @typedoc """
  Type describing actions requested by the VariantSelector to be executed
  by the TrackReceiver.
  """
  @type selector_action_t() :: variant_request_action_t() | stop_track_action_t() | noop_t()

  @typedoc """
  Action returned by the VariantSelector to express desire to
  switch to the given variant
  """
  @type variant_request_action_t() ::
          {:request, Track.variant(), TrackReceiver.variant_switch_reason()}

  @typedoc """
  Action returned by the VariantSelector when, for any reason,
  we should stop receiving the track.

  After seeing this action, TrackReceiver should immediately request
  that no variant is forwarded to it.
  """
  @type stop_track_action_t() :: :stop

  @typedoc """
  Type describing an action that indicated that no action is required.

  Upon seeing this action, TrackReceiver should maintain status quo.
  """
  @type noop_t() :: :noop

  @typedoc """
  Type describing options that can be passed to `#{inspect(__MODULE__)}.new/4`

  - `:initial_target_variant` - defines a target variant. Target variant will be prioritized over other variants. Default: `:high`
  - `negotiable?` - determines if the variant selector will have a negotiable bandwidth allocation. Default: `false`
  """
  @type option_t() :: {:initial_target_variant, Track.variant()} | {:negotiable?, boolean()}

  @opaque t() :: %__MODULE__{
            track: Track.t(),
            target_variant: Track.variant(),
            current_variant: Track.variant() | :no_variant,
            queued_variant: Track.variant() | :no_variant,
            active_variants: MapSet.t(Track.variant()),
            current_allocation: non_neg_integer(),
            variant_bitrates: bitrates_t(),
            connection_allocator: pid(),
            connection_allocator_module: module()
          }

  @enforce_keys [
    :track,
    :current_allocation,
    :connection_allocator,
    :connection_allocator_module,
    :variant_bitrates
  ]
  defstruct @enforce_keys ++
              [
                :target_variant,
                queued_variant: :no_variant,
                current_variant: :no_variant,
                active_variants: MapSet.new()
              ]

  @doc """
  Creates new variant selector.
  """
  @spec new(Track.t(), module(), pid(), [option_t()]) :: t()
  def new(
        track,
        connection_allocator_module,
        connection_allocator,
        options \\ []
      ) do
    initial_target_variant = Keyword.get(options, :initial_target_variant, :high)
    negotiable? = Keyword.get(options, :negotiable?, true)

    negotiable? =
      if negotiable? and length(track.variants) < 2 do
        Membrane.Logger.warn(
          "Attempted to enable negotiability on non-negotiable track. Disabling."
        )

        false
      else
        negotiable?
      end

    variant_bitrates =
      case track.type do
        :audio -> @default_bitrates_audio
        :video -> @default_bitrates_video
      end

    initial_allocation =
      case track.type do
        :audio -> variant_bitrates[:high] * 1.1
        # FIXME revisit initial video allocation
        # in the previous version where we had
        # variant_bitrates[:low] it could happen that
        # the first variant that became active was
        # medium or high. In such a case we were releasing
        # our initial allocation and we were not sending
        # anything except audio. As a result our bandwidth
        # estimation was starting from ~50kbps
        :video -> 1.1 * variant_bitrates[:high]
      end

    connection_allocator_module.register_track_receiver(
      connection_allocator,
      initial_allocation,
      track,
      negotiable?: negotiable?
    )

    %__MODULE__{
      track: track,
      target_variant: initial_target_variant,
      connection_allocator: connection_allocator,
      variant_bitrates: variant_bitrates,
      current_allocation: initial_allocation,
      connection_allocator_module: connection_allocator_module
    }
  end

  @doc """
  Updates bitrate assigned to a variant in the selector.
  """
  @spec update_variant_bitrate(t(), Track.variant(), non_neg_integer()) :: t()
  def update_variant_bitrate(%__MODULE__{} = selector, variant, bitrate) do
    variant_bitrates = %{selector.variant_bitrates | variant => bitrate}
    %{selector | variant_bitrates: variant_bitrates}
  end

  @doc """
  Updates the bandwidth allocated to the selector.
  """
  @spec set_bandwidth_allocation(t(), bitrates_t()) :: {t(), selector_action_t()}
  def set_bandwidth_allocation(%__MODULE__{current_allocation: allocation} = selector, allocation) do
    {selector, :noop}
  end

  def set_bandwidth_allocation(%__MODULE__{} = selector, allocation) do
    {selector, action} =
      selector
      |> Map.put(:current_allocation, allocation)
      |> perform_automatic_variant_selection()

    action = add_reason(action, :other)
    {selector, action}
  end

  @doc """
  Updates the negotiability status of the Track Receiver.

  Attempting to enable negotiability of a track that has less than 2 variants will result in `RuntimeError`.
  """
  @spec set_negotiable(t(), boolean()) :: :ok
  def set_negotiable(%__MODULE__{} = selector, negotiable?) do
    if negotiable? and length(selector.track.variants) < 2 do
      raise "Cannot enable negotiability on a track that has less than 2 variants."
    else
      selector.connection_allocator_module.set_negotiability_status(
        selector.connection_allocator,
        negotiable?
      )
    end
  end

  @doc """
  Function called by TrackReceiver upon receiving `t:Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator.decrease_allocation_request/0`
  from the ConnectionAllocator
  """
  @spec decrease_allocation(t()) :: {t(), selector_action_t()}
  def decrease_allocation(%__MODULE__{} = selector) do
    reply = &send(selector.connection_allocator, {self(), {:decrease_allocation_request, &1}})

    case next_lower_variant(selector) do
      {:ok, next_variant} when next_variant != :no_variant ->
        reply.(:accept)

        {selector, action} = select_variant(selector, next_variant)
        action = add_reason(action, :low_bandwidth)
        selector = manage_allocation(selector)
        {selector, action}

      _otherwise ->
        reply.(:reject)
        {selector, :noop}
    end
  end

  @doc """
  Marks given `variant` as inactive.
  """
  @spec variant_inactive(t(), Track.variant()) :: {t(), selector_action_t()}
  def variant_inactive(selector, variant) do
    Membrane.Logger.debug("Variant inactive #{variant}")

    selector = %__MODULE__{
      selector
      | active_variants: MapSet.delete(selector.active_variants, variant)
    }

    {selector, action} =
      case selector do
        %{current_variant: ^variant} ->
          %__MODULE__{selector | current_variant: :no_variant}

        %{queued_variant: ^variant, current_variant: :no_variant} ->
          %__MODULE__{selector | queued_variant: :no_variant}

        %{queued_variant: ^variant} ->
          %__MODULE__{selector | queued_variant: :no_variant}

        _else ->
          selector
      end
      |> perform_automatic_variant_selection()

    action = add_reason(action, :variant_inactive)
    {selector, action}
  end

  @doc """
  Marks given `variant` as active.
  """
  @spec variant_active(t(), Track.variant()) :: {t(), selector_action_t()}
  def variant_active(selector, variant) do
    Membrane.Logger.debug("Variant active #{variant}")

    selector = %__MODULE__{
      selector
      | active_variants: MapSet.put(selector.active_variants, variant)
    }

    cond do
      selector.target_variant in [selector.current_variant, selector.queued_variant] ->
        {selector, :noop}

      # make sure we're selecting a target variant if it becomes active
      # and we have the bandwidth
      selector.target_variant == variant and fits_in_allocation?(selector, variant) ->
        {selector, action} = select_variant(selector, variant)
        action = add_reason(action, :other)
        selector = manage_allocation(selector)
        {selector, action}

      # we're waiting for target, automatically select a variant
      true ->
        selector
        |> perform_automatic_variant_selection()
        |> case do
          # TODO: don't ignore stop action when RTC Engine supports it
          {selector, :stop} -> {selector, :noop}
          {selector, action} -> {selector, add_reason(action, :other)}
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
    %__MODULE__{selector | current_variant: variant, queued_variant: :no_variant}
    |> manage_allocation()
  end

  def set_current_variant(%__MODULE__{} = selector, variant) do
    %__MODULE__{selector | current_variant: variant}
    |> manage_allocation()
  end

  @doc """
  Sets the target variant that should be selected whenever it is active.

  If the target variant is not active, we will switch to it when it becomes active again
  """
  @spec set_target_variant(t(), Track.variant()) :: {t(), selector_action_t()}
  def set_target_variant(selector, variant) do
    selector = %__MODULE__{selector | target_variant: variant}

    if variant in selector.active_variants and fits_in_allocation?(selector, variant) do
      {selector, action} = select_variant(selector, variant)
      action = add_reason(action, :other)
      selector = manage_allocation(selector)
      {selector, action}
    else
      Membrane.Logger.debug("""
      Requested variant #{inspect(variant)} is either inactive, or we don't have enough bandwidth to use it. Saving it as target.
      It will be requested once it becomes active or we have enough bandwidth
      """)

      {manage_allocation(selector), :noop}
    end
  end

  defp select_variant(%__MODULE__{} = selector) do
    variant = best_active_variant(selector)
    Membrane.Logger.debug("Best active variant: #{inspect(variant)}")

    select_variant(selector, variant)
  end

  defp select_variant(selector, :no_variant) do
    Membrane.Logger.debug("No active variant.")
    selector = %__MODULE__{selector | current_variant: :no_variant, queued_variant: :no_variant}
    {selector, :stop}
  end

  defp select_variant(
         %__MODULE__{current_variant: variant, queued_variant: :no_variant} = selector,
         variant
       ) do
    Membrane.Logger.debug("Requested currently used variant #{variant}. Ignoring.")
    {selector, :noop}
  end

  defp select_variant(%__MODULE__{current_variant: variant} = selector, variant) do
    Membrane.Logger.debug("""
    Requested variant: #{inspect(variant)} which is currently used but while waiting
    for keyframe for queued_variant #{inspect(selector.queued_variant)}.
    Clearing queued_variant #{inspect(selector.queued_variant)}
    """)

    selector = %__MODULE__{selector | queued_variant: :no_variant}
    {selector, :noop}
  end

  defp select_variant(%__MODULE__{queued_variant: variant} = selector, variant) do
    Membrane.Logger.debug(" Requested the variant that has already been queued. Ignoring")
    {selector, :noop}
  end

  defp select_variant(selector, variant) do
    Membrane.Logger.debug("Enqueuing variant #{inspect(variant)}.")
    selector = %__MODULE__{selector | queued_variant: variant}
    {selector, {:request, variant}}
  end

  defp best_active_variant(selector) do
    selector.active_variants
    |> sort_variants()
    |> Enum.filter(&fits_in_allocation?(selector, &1))
    |> List.first()
    |> then(&(&1 || :no_variant))
  end

  defp next_lower_variant(
         %__MODULE__{current_variant: current_variant, queued_variant: queued_variant} = selector
       ) do
    pivot = if queued_variant != :no_variant, do: queued_variant, else: current_variant

    selector.active_variants
    |> MapSet.put(:no_variant)
    |> sort_variants()
    |> Enum.drop_while(&(&1 != pivot))
    |> case do
      [^pivot, next_variant | _rest] -> {:ok, next_variant}
      _otherwise -> {:error, :doesnt_exist}
    end
  end

  # This function clause makes sure that we're no longer looking for a better variant
  # when we  are already using target variant
  defp next_desired_variant(%__MODULE__{target_variant: target_variant} = selector)
       when target_variant in [selector.current_variant, selector.queued_variant],
       do: {:error, :doesnt_exist}

  defp next_desired_variant(
         %__MODULE__{current_variant: current_variant, queued_variant: queued_variant} = selector
       ) do
    pivot = if queued_variant != :no_variant, do: queued_variant, else: current_variant

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

  defp perform_automatic_variant_selection(%__MODULE__{} = selector) do
    {selector, action} = select_variant(selector)
    selector = manage_allocation(selector)
    {selector, action}
  end

  defp manage_allocation(%__MODULE__{} = selector) do
    current_variant_bitrate = selector.variant_bitrates[selector.current_variant]
    queued_variant_bitrate = selector.variant_bitrates[selector.queued_variant]

    required_bitrate =
      [current_variant_bitrate, queued_variant_bitrate]
      |> Enum.max()

    next_variant = next_desired_variant(selector)

    Membrane.Logger.debug("Next desired variant #{inspect(next_variant)}")

    cond do
      # If we're not having that much margin left in current allocation,
      # try to request higher allocation
      # It is very important that this clause takes precedence
      # over the one that aims to increase quality.
      # If we don't have enough bandwidth to maintain current quality,
      # don't bother with better quality, try to salvage current quality
      required_bitrate > 0.95 * selector.current_allocation ->
        Membrane.Logger.debug(
          "Requesting #{required_bitrate / 1024 * 1.1} kbps from connection prober as a mean to maintain current quality"
        )

        selector.connection_allocator_module.request_allocation(
          selector.connection_allocator,
          required_bitrate * 1.1
        )

        selector

      required_bitrate * 1.2 < selector.current_allocation ->
        Membrane.Logger.debug(
          "Requesting #{required_bitrate / 1024 * 1.1} kbps from connection prober to free unused bitrate"
        )

        selector.connection_allocator_module.request_allocation(
          selector.connection_allocator,
          required_bitrate * 1.1
        )

        manage_allocation(%{selector | current_allocation: required_bitrate * 1.1})

      # If there is a next variant that we want, let's try to request an allocation for it
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

        selector

      true ->
        selector
    end
  end

  defp fits_in_allocation?(selector, variant) do
    selector.variant_bitrates[variant] * 1.1 <= selector.current_allocation
  end

  defp add_reason({:request, variant}, reason), do: {:request, variant, reason}
  defp add_reason(action, _reason), do: action
end
