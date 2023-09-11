defmodule ExSDP.Address do
  @moduledoc """
  Module representing address used in connection-data and origin.
  """

  @type fqdn() :: {:IP4 | :IP6, binary()}
  @type t() :: :inet.ip_address() | fqdn()

  @spec parse_address(binary()) :: {:ok, t()} | {:error, :invalid_address}
  def parse_address(address) do
    case address |> to_charlist() |> :inet.parse_address() do
      {:ok, address} -> {:ok, address}
      # for fqdn
      {:error, :einval} -> {:ok, address}
    end
  end

  @spec parse_addrtype(binary()) :: {:ok, :IP4 | :IP6} | {:error, :invalid_addrtype}
  def parse_addrtype("IP4" = addrtype), do: {:ok, String.to_atom(addrtype)}
  def parse_addrtype("IP6" = addrtype), do: {:ok, String.to_atom(addrtype)}
  def parse_addrtype(_addrtype), do: {:error, :invalid_addrtype}

  @spec serialize_address(t()) :: binary()
  def serialize_address({addrtype, fqdn}) when addrtype in [:IP4, :IP6] and is_binary(fqdn),
    do: fqdn

  def serialize_address(address), do: :inet.ntoa(address)

  @spec get_addrtype(t()) :: :IP4 | :IP6
  def get_addrtype({_a, _b, _c, _d}), do: :IP4
  def get_addrtype({_a, _b, _c, _d, _e, _f, _g, _h}), do: :IP6
  def get_addrtype({addrtype, _fqdn}) when addrtype in [:IP4, :IP6], do: addrtype
end
