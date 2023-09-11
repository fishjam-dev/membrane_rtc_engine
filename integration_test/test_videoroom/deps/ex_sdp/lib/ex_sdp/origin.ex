defmodule ExSDP.Origin do
  @moduledoc """
  This module represents the Origin field of SDP that represents the originator of the session.

  If the username is set to `-` the originating host does not support the concept of user IDs.

  The username MUST NOT contain spaces.

  For more details please see [RFC4566 Section 5.2](https://tools.ietf.org/html/rfc4566#section-5.2)
  """
  use Bunch.Access

  alias ExSDP.{Address, Utils}

  @enforce_keys [
    :session_id,
    :session_version,
    :address
  ]
  defstruct [username: "-", network_type: "IN"] ++ @enforce_keys

  @type t :: %__MODULE__{
          username: binary(),
          session_id: integer(),
          session_version: integer(),
          network_type: binary(),
          address: Address.t()
        }

  @doc """
  Returns new origin struct.

  By default:
  * `username` is `-`
  * `session_id` is random 64 bit number
  * `session_version` is `0`
  * `address` is `{127, 0, 0, 1}`
  """
  @spec new(
          username: binary(),
          session_id: integer(),
          session_version: integer(),
          address: Address.t()
        ) :: t()
  def new(opts \\ []) do
    %__MODULE__{
      username: Keyword.get(opts, :username, "-"),
      session_id: Keyword.get(opts, :session_id, generate_random()),
      session_version: Keyword.get(opts, :session_version, 0),
      address: Keyword.get(opts, :address, {127, 0, 0, 1})
    }
  end

  @spec parse(binary()) ::
          {:ok, t()} | {:error, :invalid_addrtype | :invalid_address}
  def parse(origin) do
    with {:ok, [username, sess_id, sess_version, nettype, addrtype, address]} <-
           Utils.split(origin, " ", 6),
         {:ok, addrtype} <- Address.parse_addrtype(addrtype),
         {:ok, address} <- Address.parse_address(address) do
      # check whether fqdn
      address = if is_binary(address), do: {addrtype, address}, else: address

      origin = %__MODULE__{
        username: username,
        session_id: String.to_integer(sess_id),
        session_version: String.to_integer(sess_version),
        network_type: nettype,
        address: address
      }

      {:ok, origin}
    else
      {:error, _reason} = error -> error
    end
  end

  @doc """
  Increments `session_version` field.

  Can be used while sending offer/answer again.
  """
  @spec bump_version(t()) :: {:ok, t()}
  def bump_version(origin), do: {:ok, %{origin | session_version: origin.session_version + 1}}

  defp generate_random(), do: :crypto.strong_rand_bytes(7) |> :binary.decode_unsigned()
end

defimpl String.Chars, for: ExSDP.Origin do
  alias ExSDP.Address

  @impl true
  def to_string(origin) do
    """
    #{origin.username} \
    #{origin.session_id} \
    #{origin.session_version} \
    #{origin.network_type} \
    #{Address.get_addrtype(origin.address)} \
    #{Address.serialize_address(origin.address)}\
    """
  end
end
