defmodule Membrane.RTCP.FeedbackPacket.AFB do
  @moduledoc """
  [Application Layer Feedback](https://datatracker.ietf.org/doc/html/rfc4585#section-6.4) packets.

  They use PT=PSFB (206) & FMT=15.
  Since the message must be handled at the application layer, the struct simply wraps a binary content of message
  """

  @behaviour Membrane.RTCP.FeedbackPacket

  @enforce_keys [:message]
  defstruct @enforce_keys

  @impl true
  def decode(binary) do
    {:ok, %__MODULE__{message: binary}}
  end

  @impl true
  def encode(%__MODULE__{message: message}) when is_binary(message) do
    message
  end
end
