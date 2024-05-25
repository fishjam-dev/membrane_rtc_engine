defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VariantTracker do
  @moduledoc false
  # Module responsible for tracking variant activity.
  #
  # It is heavily inspired by livekit StreamTracker:
  # https://github.com/livekit/livekit-server/blob/f3572d2654dd5d1c276c9ab20e4b7bbd2184992e/pkg/sfu/streamtracker.go
  #
  # When client does not have enough bandwidth to send all track variants it can
  # disable some of them. This is not signalled so SFU has to track variant activity.

  require Membrane.Logger

  @type t :: %__MODULE__{
          variant: String.t(),
          status: :active | :inactive | :muted,
          samples: non_neg_integer(),
          activity_cycles: non_neg_integer(),
          inactivity_cycles: non_neg_integer(),
          required_samples: non_neg_integer(),
          required_activity_cycles: non_neg_integer(),
          required_inactivity_cycles: non_neg_integer()
        }

  @enforce_keys [
    :variant,
    :required_samples,
    :required_activity_cycles,
    :required_inactivity_cycles
  ]
  defstruct @enforce_keys ++
              [
                status: :active,
                samples: 0,
                activity_cycles: 0,
                inactivity_cycles: 0
              ]

  @spec new(String.t(), non_neg_integer(), non_neg_integer(), non_neg_integer()) :: t()
  def new(
        variant,
        required_samples \\ 5,
        required_activity_cycles \\ 10,
        required_inactivity_cycles \\ 1
      ) do
    %__MODULE__{
      variant: variant,
      required_samples: required_samples,
      required_activity_cycles: required_activity_cycles,
      required_inactivity_cycles: required_inactivity_cycles
    }
  end

  @spec increment_samples(t()) :: t()
  def increment_samples(tracker) do
    %__MODULE__{tracker | samples: tracker.samples + 1}
  end

  @doc """
  Checks if variant changed its status from last check.

  Returns `{:ok, t()}` if variant didn't change its status and
  `{:status_changed, t(), :inactive | :active}` otherwise.
  This function also resets VariantTracker state.
  """
  @spec check_variant_status(t()) :: {:ok, t()} | {:status_changed, t(), :inactive | :active}
  def check_variant_status(tracker) do
    if tracker.samples < tracker.required_samples do
      tracker = %__MODULE__{
        tracker
        | samples: 0,
          activity_cycles: 0,
          inactivity_cycles: tracker.inactivity_cycles + 1
      }

      maybe_inactive(tracker)
    else
      tracker = %__MODULE__{
        tracker
        | samples: 0,
          inactivity_cycles: 0,
          activity_cycles: tracker.activity_cycles + 1
      }

      maybe_active(tracker)
    end
  end

  @spec set_variant_muted(t()) :: {:ok, t()} | {:status_changed, t(), :muted}
  def set_variant_muted(tracker) do
    if tracker.status == :active do
      Membrane.Logger.debug("Variant #{inspect(tracker.variant)} is muted.")
      tracker = %__MODULE__{tracker | status: :muted, inactivity_cycles: 0}
      {:status_changed, tracker, :muted}
    else
      {:ok, tracker}
    end
  end

  @spec set_variant_unmuted(t()) :: {:ok, t()} | {:status_changed, t(), :active}
  def set_variant_unmuted(tracker) do
    if tracker.status == :muted do
      Membrane.Logger.debug("Variant #{inspect(tracker.variant)} is unmuted.")
      tracker = %__MODULE__{tracker | status: :active, activity_cycles: 0}
      {:status_changed, tracker, :active}
    else
      {:ok, tracker}
    end
  end

  @doc """
  Resets VariantTracker state.
  """
  @spec reset(t()) :: t()
  def reset(tracker) do
    %__MODULE__{tracker | samples: 0, activity_cycles: 0, inactivity_cycles: 0}
  end

  defp maybe_inactive(tracker) do
    if tracker.status == :active and
         tracker.inactivity_cycles == tracker.required_inactivity_cycles do
      Membrane.Logger.debug("Variant #{inspect(tracker.variant)} is inactive.")
      tracker = %__MODULE__{tracker | status: :inactive, inactivity_cycles: 0}
      {:status_changed, tracker, :inactive}
    else
      {:ok, tracker}
    end
  end

  defp maybe_active(tracker) do
    if tracker.status == :inactive and tracker.activity_cycles == tracker.required_activity_cycles do
      Membrane.Logger.debug("Variant #{inspect(tracker.variant)} is active.")
      tracker = %__MODULE__{tracker | status: :active, activity_cycles: 0}
      {:status_changed, tracker, :active}
    else
      {:ok, tracker}
    end
  end
end
