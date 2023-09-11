defmodule ExSDP.Attribute.SSRC do
  @moduledoc """
  This module represents ssrc (RFC 5576).
  """
  alias ExSDP.Utils

  @enforce_keys [:id, :attribute]
  defstruct @enforce_keys ++ [:value]

  @type t :: %__MODULE__{id: non_neg_integer(), attribute: binary(), value: binary() | nil}

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :ssrc

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_ssrc}
  def parse(ssrc) do
    with [id, attribute] <- String.split(ssrc, " ", parts: 2),
         {:ok, id} <- Utils.parse_numeric_string(id) do
      case String.split(attribute, ":", parts: 2) do
        [attribute, value] -> {:ok, %__MODULE__{id: id, attribute: attribute, value: value}}
        [attribute] -> {:ok, %__MODULE__{id: id, attribute: attribute}}
        _invalid_ssrc -> {:error, :invalid_ssrc}
      end
    else
      _invalid_ssrc -> {:error, :invalid_ssrc}
    end
  end
end

defimpl String.Chars, for: ExSDP.Attribute.SSRC do
  alias ExSDP.Attribute.SSRC

  @impl true
  def to_string(%SSRC{id: id, attribute: attribute, value: nil}), do: "ssrc:#{id} #{attribute}"

  def to_string(%SSRC{id: id, attribute: attribute, value: value}),
    do: "ssrc:#{id} #{attribute}:#{value}"
end
