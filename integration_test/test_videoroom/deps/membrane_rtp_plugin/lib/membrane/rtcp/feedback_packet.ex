defmodule Membrane.RTCP.FeedbackPacket do
  @moduledoc """
  Abstraction and generic encoding/decoding functionality for
  [RTCP payload-specific feedback packets](https://datatracker.ietf.org/doc/html/rfc5104#section-4.3).
  """

  @behaviour Membrane.RTCP.Packet

  alias Membrane.RTP

  @enforce_keys [:origin_ssrc, :payload]
  defstruct @enforce_keys ++ [target_ssrc: 0]

  @type t :: %__MODULE__{
          origin_ssrc: RTP.ssrc_t(),
          target_ssrc: RTP.ssrc_t(),
          payload: struct
        }

  @callback decode(binary()) :: {:ok, struct()} | {:error, any()}

  @callback encode(struct()) :: binary()

  @packet_type_payload BiMap.new(%{
                         1 => __MODULE__.PLI,
                         4 => __MODULE__.FIR,
                         15 => __MODULE__.AFB
                       })

  @impl true
  def decode(<<origin_ssrc::32, target_ssrc::32, payload::binary>>, packet_type) do
    with {:ok, module} <- BiMap.fetch(@packet_type_payload, packet_type),
         {:ok, payload} <- module.decode(payload) do
      {:ok, %__MODULE__{origin_ssrc: origin_ssrc, target_ssrc: target_ssrc, payload: payload}}
    else
      :error -> {:error, {:unknown_feedback_packet_type, packet_type}}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def decode(_binary, _packet_type) do
    {:error, :malformed_packet}
  end

  @impl true
  def encode(%__MODULE__{} = packet) do
    %module{} = packet.payload
    packet_type = BiMap.fetch_key!(@packet_type_payload, module)

    {<<packet.origin_ssrc::32, packet.target_ssrc::32, module.encode(packet.payload)::binary>>,
     packet_type}
  end
end
