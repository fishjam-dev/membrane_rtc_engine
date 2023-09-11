defmodule ExSDP.ConnectionData do
  @moduledoc """
  This module represents the Connection Information.

  The address can be represented by either:
   - IPv4 address
   - IPv6 address
   - FQDN (Fully Qualified Domain Name)

  In the case of IPv4 and IPv6 multicast addresses there can be more than one
  parsed from single SDP field if it is described using slash notation.

  Sessions using an IPv4 multicast connection address MUST also have
  a time to live (TTL) value present in addition to the multicast
  address.

  For more details please see [RFC4566 Section 5.7](https://tools.ietf.org/html/rfc4566#section-5.7)
  """
  use Bunch
  use Bunch.Access

  alias ExSDP.{Address, Utils}

  @enforce_keys [:address]
  defstruct @enforce_keys ++ [:address_count, :ttl, network_type: "IN"]

  @type t :: %__MODULE__{
          address: Address.t(),
          address_count: pos_integer() | nil,
          ttl: 0..255 | nil,
          network_type: binary()
        }

  @type reason ::
          :invalid_addrtype
          | :invalid_address
          | :ip6_cannot_have_ttl
          | :invalid_ttl_or_address_count

  @spec parse(binary()) :: {:ok, t()} | {:error, reason}
  def parse(connection) do
    with {:ok, [nettype, addrtype, connection_address]} <- Utils.split(connection, " ", 3),
         {:ok, addrtype} <- Address.parse_addrtype(addrtype),
         [address | ttl_with_address_count] <- String.split(connection_address, "/", parts: 2),
         {:ok, address} <- Address.parse_address(address),
         {:ok, ttl, address_count} <-
           parse_ttl_with_address_count(ttl_with_address_count, addrtype) do
      # check whether fqdn
      address = if is_binary(address), do: {addrtype, address}, else: address

      connection_data = %__MODULE__{
        address: address,
        address_count: address_count,
        ttl: ttl,
        network_type: nettype
      }

      {:ok, connection_data}
    else
      {:error, _reason} = error -> error
      _invalid_address -> {:error, :invalid_address}
    end
  end

  defp parse_ttl_with_address_count([], _addrtype), do: {:ok, nil, nil}
  defp parse_ttl_with_address_count([""], _addrtype), do: {:error, :invalid_ttl_or_address_count}

  defp parse_ttl_with_address_count([ttl_with_address_count], :IP4) do
    with [ttl] <- String.split(ttl_with_address_count, "/"),
         {:ok, ttl} when ttl in 0..255 <- Utils.parse_numeric_string(ttl) do
      {:ok, ttl, nil}
    else
      [ttl, address_count] ->
        with {:ok, ttl} <- Utils.parse_numeric_string(ttl),
             {:ok, address_count} <- Utils.parse_numeric_string(address_count) do
          {:ok, ttl, address_count}
        else
          _invalid_ttl_or_address_count -> {:error, :invalid_ttl_or_address_count}
        end

      _invalid_ttl_or_address_count ->
        {:error, :invalid_ttl_or_address_count}
    end
  end

  defp parse_ttl_with_address_count([ttl_with_address_count], :IP6) do
    case String.split(ttl_with_address_count, "/") do
      [address_count] ->
        case Utils.parse_numeric_string(address_count) do
          {:ok, address_count} -> {:ok, nil, address_count}
          _invalid_ttl_or_address_count -> {:error, :invalid_ttl_or_address_count}
        end

      [_ttl, _address_count] ->
        {:error, :ip6_cannot_have_ttl}

      _invalid_ttl_or_address_count ->
        {:error, :invalid_ttl_or_address_count}
    end
  end
end

defimpl String.Chars, for: ExSDP.ConnectionData do
  alias ExSDP.{Address, ConnectionData}

  @impl true
  def to_string(%ConnectionData{} = connection) do
    """
    #{connection.network_type} \
    #{Address.get_addrtype(connection.address)} \
    #{Address.serialize_address(connection.address)}\
    #{serialize_ttl_with_address_count(connection.ttl, connection.address_count)}\
    """
  end

  defp serialize_ttl_with_address_count(nil, nil), do: ""
  defp serialize_ttl_with_address_count(ttl, nil), do: "/#{ttl}"
  defp serialize_ttl_with_address_count(nil, address_count), do: "/#{address_count}"
  defp serialize_ttl_with_address_count(ttl, address_count), do: "/#{ttl}/#{address_count}"
end
