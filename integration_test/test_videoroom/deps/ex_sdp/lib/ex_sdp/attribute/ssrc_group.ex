defmodule ExSDP.Attribute.SSRCGroup do
  @moduledoc """
  This module represents `ssrc-group` [RFC 5576](https://datatracker.ietf.org/doc/html/rfc5576#section-4.2) attribute.
  """
  alias ExSDP.Utils

  @enforce_keys [:semantics, :ssrcs]
  defstruct @enforce_keys

  @type t :: %__MODULE__{semantics: binary(), ssrcs: [non_neg_integer()]}

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :ssrc_group

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_ssrc | :invalid_ssrc_group}
  def parse(ssrc_group) do
    with [semantics | ssrc_ids] when ssrc_ids != [] <- String.split(ssrc_group, " "),
         {:ok, ssrcs} <- Bunch.Enum.try_map(ssrc_ids, &Utils.parse_numeric_string/1) do
      {:ok, %__MODULE__{semantics: semantics, ssrcs: ssrcs}}
    else
      [_invalid] -> {:error, :invalid_ssrc_group}
      _invalid_ssrc -> {:error, :invalid_ssrc}
    end
  end
end

defimpl String.Chars, for: ExSDP.Attribute.SSRCGroup do
  alias ExSDP.Attribute.SSRCGroup

  @impl true
  def to_string(%SSRCGroup{semantics: semantics, ssrcs: ssrcs}) do
    "ssrc-group:#{semantics} #{Enum.join(ssrcs, " ")}"
  end
end
