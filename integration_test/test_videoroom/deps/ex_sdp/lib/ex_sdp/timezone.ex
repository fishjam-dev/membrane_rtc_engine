defmodule ExSDP.Timezone do
  @moduledoc """
  This module groups multiple SDP Timezone Correction used
  for translating base time for rebroadcasts.
  """
  use Bunch.Access

  alias __MODULE__.Correction

  defstruct corrections: []

  @type t :: %__MODULE__{
          corrections: [Correction.t()]
        }

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_timezone}
  def parse(timezones) do
    case String.split(timezones, " ") do
      list when rem(length(list), 2) == 0 -> parse_timezones(list)
      _invalid_timezone -> {:error, :invalid_timezone}
    end
  end

  defp parse_timezones(timezone_corrections) do
    parsed =
      timezone_corrections
      |> Enum.chunk_every(2)
      |> Bunch.Enum.try_map(fn [adjustment_time, offset] ->
        Correction.parse("#{adjustment_time} #{offset}")
      end)

    with {:ok, corrections} when is_list(corrections) <- parsed do
      {:ok, %__MODULE__{corrections: corrections}}
    end
  end
end

defimpl String.Chars, for: ExSDP.Timezone do
  alias ExSDP.Timezone

  @impl true
  def to_string(%Timezone{corrections: []}), do: ""

  def to_string(%Timezone{corrections: corrections}),
    do: Enum.map_join(corrections, " ", &Kernel.to_string/1)
end
