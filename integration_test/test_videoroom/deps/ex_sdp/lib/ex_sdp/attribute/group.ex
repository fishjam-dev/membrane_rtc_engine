defmodule ExSDP.Attribute.Group do
  @moduledoc """
  This module represents group (RFC 5888).
  """

  @enforce_keys [:semantics, :mids]
  defstruct @enforce_keys

  @type t :: %__MODULE__{semantics: String.t(), mids: [String.t()]}

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :group

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_group}
  def parse(group) do
    with [semantics | mids] <- String.split(group, " "),
         # check against any redundant white spaces
         false <- Enum.any?(mids, &(String.match?(&1, ~r/\s+/) or &1 == "")) do
      {:ok, %__MODULE__{semantics: semantics, mids: mids}}
    else
      _invalid_group -> {:error, :invalid_group}
    end
  end
end

defimpl String.Chars, for: ExSDP.Attribute.Group do
  alias ExSDP.Attribute.Group

  @impl true
  def to_string(%Group{semantics: semantics, mids: mids}),
    do: "group:#{semantics} #{Enum.join(mids, " ")}" |> String.trim()
end
