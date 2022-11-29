defmodule Membrane.RTC.Engine.BitrateEstimator do
  @moduledoc false

  alias Membrane.RTC.Engine.Track.BitrateEstimation
  alias Membrane.{Buffer, Time}

  @enforce_keys [:last_estimation_time]
  defstruct @enforce_keys ++
              [
                sizes: []
              ]

  @opaque t() :: %__MODULE__{
            sizes: [{integer(), non_neg_integer()}],
            last_estimation_time: integer()
          }

  @spec new() :: t()
  def new() do
    %__MODULE__{
      last_estimation_time: get_timestamp()
    }
  end

  @spec process(t(), Buffer.t()) :: t()
  def process(state, buffer) do
    entry = byte_size(buffer.payload) * 8

    Map.update!(state, :sizes, &[entry | &1])
  end

  @spec estimate(t()) :: {:ok, BitrateEstimation.t(), t()} | {:error, :not_enough_data}
  def estimate(state) do
    start = state.last_estimation_time
    stop = get_timestamp()
    duration = Time.as_seconds(stop - start)

    cond do
      duration == 0 ->
        {:error, :not_enough_data}

      state.sizes == [] ->
        estimation = %BitrateEstimation{estimation: 0, error: 0}
        {:ok, estimation, %{state | last_estimation_time: stop}}

      true ->
        estimation = %BitrateEstimation{
          estimation:
            state.sizes
            |> Enum.sum()
            |> Ratio.new(duration)
            |> Ratio.to_float(),
          error:
            state.sizes
            |> Statistics.stdev()
            |> then(&(&1 * length(state.sizes)))
            |> Ratio.new(duration)
            |> Ratio.to_float()
        }

        {:ok, estimation, %{state | last_estimation_time: stop, sizes: []}}
    end
  end

  defp get_timestamp(), do: :millisecond |> System.monotonic_time() |> Time.milliseconds()
end
