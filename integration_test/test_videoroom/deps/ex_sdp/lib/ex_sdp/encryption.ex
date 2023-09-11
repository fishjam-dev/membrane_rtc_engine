defmodule ExSDP.Encryption do
  @moduledoc """
  This module represents the Encryption field of SDP that
  stores encryption key or acquisition method of such key.

  Session key should be present IFF the transport medium
  is secure.

  For more details please see [RFC4566 Section 5.12](https://tools.ietf.org/html/rfc4566#section-5.12)
  """
  use Bunch.Access

  @enforce_keys [:method]
  defstruct @enforce_keys ++ [:key]

  @type methods :: :prompt | :base64 | :clear | :prompt | :uri

  @type t :: %__MODULE__{
          method: methods(),
          key: binary() | nil
        }

  @spec parse(binary()) :: {:ok, t()} | {:error, :unsupported_method}
  def parse(definition) do
    with {method, key} <- parse_definition(definition),
         {:ok, method} <- method_to_atom(method) do
      encryption = %__MODULE__{
        method: method,
        key: key
      }

      {:ok, encryption}
    end
  end

  defp parse_definition(definition) do
    case String.split(definition, ":", parts: 2) do
      [method] -> {method, nil}
      [method, key] -> {method, key}
    end
  end

  defp method_to_atom(method)
  defp method_to_atom("prompt"), do: {:ok, :prompt}
  defp method_to_atom("clear"), do: {:ok, :clear}
  defp method_to_atom("base64"), do: {:ok, :base64}
  defp method_to_atom("uri"), do: {:ok, :uri}
  defp method_to_atom(_unknown), do: {:error, :unsupported_method}
end

defimpl String.Chars, for: ExSDP.Encryption do
  @impl true
  def to_string(encryption) do
    key = if encryption.key, do: ":" <> encryption.key, else: ""
    "#{encryption.method}#{key}"
  end
end
