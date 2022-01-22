defmodule Membrane.RTC.Engine.Endpoint.WebRTC.EncodingTracker do
  @moduledoc false
  # Module responsible for tracking encoding activity.
  #
  # It is heavily inspired by livekit StreamTracker
  # https://github.com/livekit/livekit-server/blob/f3572d2654dd5d1c276c9ab20e4b7bbd2184992e/pkg/sfu/streamtracker.go
  #
  # When client does not have enough bandwidth to send
  # all encodings it can disable some of them.
  # This is not signalised so that SFU has to track
  # encoding activity.

  require Membrane.Logger

  @type t :: %__MODULE__{
          encoding: String.t(),
          status: :active | :inactive,
          samples: non_neg_integer(),
          cycles: non_neg_integer(),
          required_samples: non_neg_integer(),
          required_cycles: non_neg_integer()
        }

  @enforce_keys [:encoding, :status, :samples, :cycles, :required_samples, :required_cycles]
  defstruct @enforce_keys

  @spec new(String.t(), non_neg_integer(), non_neg_integer()) :: t()
  def new(encoding, required_samples \\ 5, required_cycles \\ 10) do
    %__MODULE__{
      encoding: encoding,
      status: :active,
      samples: 0,
      cycles: 0,
      required_samples: required_samples,
      required_cycles: required_cycles
    }
  end

  @spec update(t()) :: t()
  def update(tracker) do
    %__MODULE__{tracker | samples: tracker.samples + 1}
  end

  @spec check(t()) :: {:ok, t()} | {:status_changed, t(), :inactive | :active}
  def check(tracker) do
    if tracker.samples < tracker.required_samples do
      tracker = %__MODULE__{tracker | samples: 0, cycles: 0}
      maybe_inactive(tracker)
    else
      tracker = %__MODULE__{tracker | samples: 0, cycles: tracker.cycles + 1}
      maybe_active(tracker)
    end
  end

  defp maybe_inactive(tracker) do
    if tracker.status == :active do
      Membrane.Logger.info("Encoding #{inspect(tracker.encoding)} is inactive.")
      tracker = %__MODULE__{tracker | status: :inactive}
      {:status_changed, tracker, :inactive}
    else
      {:ok, tracker}
    end
  end

  defp maybe_active(tracker) do
    if tracker.status == :inactive and tracker.cycles == tracker.required_cycles do
      Membrane.Logger.info("Encoding #{inspect(tracker.encoding)} is active.")
      tracker = %__MODULE__{tracker | status: :active, cycles: 0}
      {:status_changed, tracker, :active}
    else
      {:ok, tracker}
    end
  end
end
