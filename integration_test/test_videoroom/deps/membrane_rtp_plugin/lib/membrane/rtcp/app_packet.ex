defmodule Membrane.RTCP.AppPacket do
  @moduledoc """
  Parses RTCP Application-defined (APP) packets
  defined in [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.7)
  """

  @behaviour Membrane.RTCP.Packet

  defstruct [:subtype, :ssrc, :name, :data]

  @type t :: %__MODULE__{
          subtype: non_neg_integer(),
          ssrc: non_neg_integer(),
          name: String.t(),
          data: binary()
        }

  @impl true
  def decode(<<ssrc::32, name::bitstring-size(32), data::binary>>, subtype) do
    {:ok,
     %__MODULE__{
       subtype: subtype,
       ssrc: ssrc,
       name: name,
       data: data
     }}
  end

  @impl true
  def encode(%__MODULE__{
        subtype: subtype,
        ssrc: ssrc,
        name: name,
        data: data
      }) do
    {<<ssrc::32, name::bitstring-size(32), data::binary>>, subtype}
  end
end
