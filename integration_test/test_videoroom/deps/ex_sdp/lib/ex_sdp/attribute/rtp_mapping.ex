defmodule ExSDP.Attribute.RTPMapping do
  @moduledoc """
  This module represents RTP mapping, a media-level attribute named `rtpmap`.

  This attribute is defined in [Chapter 6.6 of RFC8866 (SDP)](https://datatracker.ietf.org/doc/html/rfc8866#section-6.6)
  """
  use Bunch.Access

  alias ExSDP.Utils

  @enforce_keys [:payload_type, :encoding, :clock_rate]
  defstruct @enforce_keys ++ [:params]

  @type t :: %__MODULE__{
          payload_type: payload_type_t(),
          encoding: binary(),
          clock_rate: non_neg_integer(),
          params: non_neg_integer() | nil
        }

  @type payload_type_t() :: 0..127

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :rtpmap

  @spec parse(binary(), opts :: []) ::
          {:ok, t()}
          | {:error, :string_nan | :invalid_pt | :only_audio_can_have_params | :invalid_rtpmap}
  def parse(mapping, opts) do
    with [payload_type, encoding | _] <- String.split(mapping, " "),
         [encoding_name, clock_rate | params] <- String.split(encoding, "/"),
         {:ok, payload_type} <- Utils.parse_payload_type(payload_type),
         {:ok, clock_rate} <- Utils.parse_numeric_string(clock_rate),
         {:ok, params} <- parse_params(params, opts) do
      mapping = %__MODULE__{
        payload_type: payload_type,
        encoding: encoding_name,
        clock_rate: clock_rate,
        params: params
      }

      {:ok, mapping}
    else
      {:error, reason} -> {:error, reason}
      _invalid_rtpmap -> {:error, :invalid_rtpmap}
    end
  end

  defp parse_params([channels], media_type: :audio), do: {:ok, String.to_integer(channels)}
  defp parse_params([], media_type: :audio), do: {:ok, 1}
  defp parse_params([], media_type: _media_type), do: {:ok, nil}
  defp parse_params(_params, media_type: _media_type), do: {:error, :only_audio_can_have_params}
end

defimpl String.Chars, for: ExSDP.Attribute.RTPMapping do
  alias ExSDP.Attribute.RTPMapping

  @impl true
  def to_string(mapping) do
    "rtpmap:#{mapping.payload_type} #{mapping.encoding}/#{mapping.clock_rate}#{serialize_params(mapping)}"
  end

  defp serialize_params(%RTPMapping{params: nil}), do: ""
  defp serialize_params(%RTPMapping{params: 1}), do: ""
  defp serialize_params(%RTPMapping{params: params}), do: "/" <> Kernel.to_string(params)
end
