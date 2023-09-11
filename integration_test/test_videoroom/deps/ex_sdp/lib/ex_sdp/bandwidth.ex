defmodule ExSDP.Bandwidth do
  @moduledoc """
  This module represents the bandwidth, a field of SDP that
  denotes the proposed bandwidth to be used by the session or media.

  For more details please see [RFC4566 Section 5.8](https://tools.ietf.org/html/rfc4566#section-5.8).
  """
  use Bunch.Access

  @enforce_keys [:type, :bandwidth]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          type: type(),
          bandwidth: non_neg_integer()
        }

  @type type :: :CT | :AS

  @supported_types ["CT", "AS"]

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_bandwidth}
  def parse(bandwidth) do
    with [type, bandwidth] <- String.split(bandwidth, ":"),
         {:ok, bandwidth} <- parse_bandwidth(bandwidth),
         {:ok, type} <- parse_type(type) do
      bandwidth = %__MODULE__{
        type: type,
        bandwidth: bandwidth
      }

      {:ok, bandwidth}
    else
      _invalid_bandwidth -> {:error, :invalid_bandwidth}
    end
  end

  defp parse_type(type) when type in @supported_types, do: {:ok, String.to_atom(type)}
  defp parse_type("X-" <> _experimental_rest), do: {:error, :experimental_not_supported}
  defp parse_type(_invalid_type), do: {:error, :invalid_type}

  defp parse_bandwidth(bandwidth) do
    with {value, ""} <- Integer.parse(bandwidth) do
      {:ok, value}
    end
  end
end

defimpl String.Chars, for: ExSDP.Bandwidth do
  @impl true
  def to_string(bandwidth), do: "#{bandwidth.type}:#{bandwidth.bandwidth}"
end
