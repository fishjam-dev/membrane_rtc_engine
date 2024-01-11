defmodule Membrane.RTC.Engine.Endpoint.SIP.RegistrarCredentials do
  @moduledoc """
  Module describing credentials needed to connect with SIP registrar server
  """

  @typedoc """
  Describes SIP registrar credentials structure.

  * `uri` - URI with address of the registrar server.
  * `username` - your username in the registrar service
  * `password` - your password in the registrar service
  """
  @type t :: %__MODULE__{
          uri: Sippet.URI.t(),
          username: String.t(),
          password: String.t()
        }

  @enforce_keys [:uri, :username, :password]
  defstruct @enforce_keys

  @doc """
  Creates a `RegistrarCredentials` struct from strings.

  The address is parsed and can be:
    - an FQDN, e.g. `"my-sip-registrar.net"`,
    - an IPv4 in string form, e.g. `"1.2.3.4"`.

  Both can have a specified port, e.g. `"5.6.7.8:9999"`.
  If not given, the default SIP port `5060` will be assumed.
  """
  @spec new(address: String.t(), username: String.t(), password: String.t()) ::
          t() | no_return()
  def new(opts) do
    uri =
      opts
      |> Keyword.fetch!(:address)
      |> then(&("sip:" <> &1))
      |> Sippet.URI.parse!()

    %__MODULE__{
      uri: uri,
      username: Keyword.fetch!(opts, :username),
      password: Keyword.fetch!(opts, :password)
    }
  end
end
