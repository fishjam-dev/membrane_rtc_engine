defmodule ExSDP.Timezone.Correction do
  @moduledoc """
  This module represents a single SDP Timezone Correction used
  for translating base time for rebroadcasts.

  For more details please see [RFC4566 Section 5.11](https://tools.ietf.org/html/rfc4566#section-5.11)
  """
  use Bunch.Access

  @enforce_keys [:adjustment_time, :offset]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          adjustment_time: non_neg_integer(),
          offset: -12..12
        }

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_timezone}
  def parse(correction) do
    case String.split(correction) do
      [adjustment_time, offset] -> wrap_correction(adjustment_time, offset)
      _invalid_timezone -> {:error, :invalid_timezone}
    end
  end

  defp wrap_correction(adjustment_time, offset) do
    with {adjustment_time, ""} <- Integer.parse(adjustment_time),
         {offset, rest} when rest == "" or rest == "h" <- Integer.parse(offset) do
      correction = %__MODULE__{
        adjustment_time: adjustment_time,
        offset: offset
      }

      {:ok, correction}
    else
      _invalid_timezone -> {:error, :invalid_timezone}
    end
  end
end

defimpl String.Chars, for: ExSDP.Timezone.Correction do
  @impl true
  def to_string(correction) do
    serialized_offset =
      if correction.offset == 0 do
        "#{correction.offset}"
      else
        "#{correction.offset}h"
      end

    "#{correction.adjustment_time} #{serialized_offset}"
  end
end
