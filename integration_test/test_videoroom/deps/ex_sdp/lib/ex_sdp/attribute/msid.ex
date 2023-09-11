defmodule ExSDP.Attribute.MSID do
  @moduledoc """
  This module represents msid (RFC 8830).
  """

  @enforce_keys [:id]
  defstruct @enforce_keys ++ [:app_data]

  @type t :: %__MODULE__{id: binary(), app_data: binary() | nil}

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :msid

  @spec new(id :: binary(), app_data :: binary() | nil) :: t()
  def new(id \\ UUID.uuid4(), app_data \\ UUID.uuid4()) do
    %__MODULE__{id: id, app_data: app_data}
  end

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_msid}
  def parse(msid) do
    case String.split(msid, " ") do
      [""] ->
        {:error, :invalid_msid}

      ["", _app_data] ->
        {:error, :invalid_msid}

      [id] ->
        {:ok, %__MODULE__{id: id}}

      [id, app_data] ->
        msid = %__MODULE__{
          id: id,
          app_data: app_data
        }

        {:ok, msid}

      _invalid_msid ->
        {:error, :invalid_msid}
    end
  end
end

defimpl String.Chars, for: ExSDP.Attribute.MSID do
  alias ExSDP.Attribute.MSID

  @impl true
  def to_string(%MSID{id: id, app_data: nil}), do: "msid:#{id}"
  def to_string(%MSID{id: id, app_data: app_data}), do: "msid:#{id} #{app_data}"
end
