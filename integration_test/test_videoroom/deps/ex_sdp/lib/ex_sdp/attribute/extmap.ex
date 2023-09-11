defmodule ExSDP.Attribute.Extmap do
  @moduledoc """
  This module represents extmap (RFC 8285).
  """
  alias ExSDP.Utils

  @enforce_keys [:id, :uri]
  defstruct @enforce_keys ++ [direction: nil, attributes: []]

  @type extension_id :: 1..14
  @type direction :: :sendonly | :recvonly | :sendrecv | :inactive | nil

  @type t :: %__MODULE__{
          id: extension_id(),
          direction: direction(),
          uri: String.t(),
          attributes: [String.t()]
        }

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :extmap

  @typedoc """
  Reason of parsing failure.
  """
  @type reason :: :invalid_extmap | :invalid_id | :invalid_direction | :invalid_uri

  @valid_directions ["sendonly", "recvonly", "sendrecv", "inactive"]

  @spec parse(binary()) :: {:ok, t()} | {:error, reason()}
  def parse(extmap) do
    with [id_direction, uri_attributes] <- String.split(extmap, " ", parts: 2),
         {:ok, {id, direction}} <- parse_id_direction(id_direction),
         {:ok, {uri, attributes}} <- parse_uri_attributes(uri_attributes) do
      {:ok, %__MODULE__{id: id, direction: direction, uri: uri, attributes: attributes}}
    else
      {:error, reason} -> {:error, reason}
      _invalid_extmap -> {:error, :invalid_extmap}
    end
  end

  defp parse_id_direction(id_direction) do
    case String.split(id_direction, "/") do
      [id, direction] ->
        with {:ok, id} <- Utils.parse_numeric_string(id),
             {:ok, direction} <- parse_direction(direction) do
          {:ok, {id, direction}}
        else
          {:error, :string_nan} -> {:error, :invalid_id}
          {:error, reason} -> {:error, reason}
        end

      [id] ->
        case Utils.parse_numeric_string(id) do
          {:ok, id} -> {:ok, {id, nil}}
          _invalid_id -> {:error, :invalid_id}
        end

      _invalid_extmap ->
        {:error, :invalid_extmap}
    end
  end

  defp parse_uri_attributes(uri_attributes) do
    case String.split(uri_attributes, " ") do
      [uri | attributes] -> {:ok, {uri, attributes}}
      _invalid_uri -> {:error, :invalid_uri}
    end
  end

  defp parse_direction(direction) when direction in @valid_directions,
    do: {:ok, String.to_atom(direction)}

  defp parse_direction(_invalid_directio), do: {:error, :invalid_direction}
end

defimpl String.Chars, for: ExSDP.Attribute.Extmap do
  alias ExSDP.Attribute.Extmap

  @impl true
  def to_string(%Extmap{id: id, direction: direction, uri: uri, attributes: attributes}) do
    maybe_direction = if direction == nil, do: "", else: "/#{Atom.to_string(direction)}"

    attributes = Enum.join(attributes, " ")

    "extmap:#{id}#{maybe_direction} #{uri} #{attributes}" |> String.trim()
  end
end
