defmodule Membrane.RTCP.FeedbackPacket.PLI do
  @moduledoc """
  Encodes and decodes [Picture Loss Indication](https://datatracker.ietf.org/doc/html/rfc4585#section-6.3.1) packets.
  """

  @behaviour Membrane.RTCP.FeedbackPacket

  defstruct []

  @impl true
  def decode(_binary) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def encode(_packet) do
    <<>>
  end
end
