defmodule Membrane.RTCP.ByePacket do
  @moduledoc """
  Parses and constructs RTCP BYE packets defined in
  [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.6)
  """

  @behaviour Membrane.RTCP.Packet

  @type t :: %__MODULE__{
          ssrcs: [non_neg_integer()],
          reason: String.t() | nil
        }

  defstruct [:ssrcs, :reason]

  @impl true
  def decode(packet, count) do
    ssrcs_size = count * 4
    <<ssrcs::binary-size(ssrcs_size), reason::binary>> = packet

    ssrcs =
      ssrcs
      |> Bunch.Binary.chunk_every(4)
      |> Enum.map(&:binary.decode_unsigned(&1))

    result = %__MODULE__{ssrcs: ssrcs, reason: make_reason(reason)}
    {:ok, result}
  end

  @impl true
  def encode(%{ssrcs: ssrcs, reason: reason}) do
    count = ssrcs |> length()
    ssrcs = ssrcs |> Enum.map_join(&<<&1::32>>)

    reason =
      case reason do
        nil ->
          <<>>

        other ->
          length = String.length(other)
          <<length::8, other::binary>>
      end

    {ssrcs <> reason, count}
  end

  defp make_reason(<<>>), do: nil
  defp make_reason(<<_length::8, reason::binary>>), do: reason
end
