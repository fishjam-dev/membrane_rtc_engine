defmodule Membrane.RTCP.Packet do
  @moduledoc """
  Functions common to all RTCP Packets
  """
  require Membrane.Logger

  alias Membrane.RTCP.{
    AppPacket,
    ByePacket,
    FeedbackPacket,
    Header,
    ReceiverReportPacket,
    SdesPacket,
    SenderReportPacket,
    TransportFeedbackPacket
  }

  alias Membrane.RTP

  @type t ::
          AppPacket.t()
          | ByePacket.t()
          | FeedbackPacket.t()
          | TransportFeedbackPacket.t()
          | ReceiverReportPacket.t()
          | SenderReportPacket.t()
          | SdesPacket.t()

  @doc """
  Decodes binary with packet body (without header) into packet struct. Used by `parse/1`
  """
  @callback decode(binary(), packet_specific :: Header.packet_specific_t()) ::
              {:ok, struct()} | {:error, atom()}

  @doc """
  Encodes packet struct into the tuple used by `serialize/1`
  """
  @callback encode(struct()) :: {body :: binary(), packet_specific :: Header.packet_specific_t()}

  @packet_type_module BiMap.new(%{
                        200 => SenderReportPacket,
                        201 => ReceiverReportPacket,
                        202 => SdesPacket,
                        203 => ByePacket,
                        204 => AppPacket,
                        205 => TransportFeedbackPacket,
                        206 => FeedbackPacket
                      })

  @doc """
  Converts packet structure into binary
  """
  @spec serialize(t() | [t()]) :: binary()
  def serialize(%packet_module{} = packet) do
    {body, packet_specific} = packet_module.encode(packet)
    packet_type = BiMap.fetch_key!(@packet_type_module, packet_module)

    header =
      %Header{packet_type: packet_type, packet_specific: packet_specific}
      |> Header.serialize(body_size: byte_size(body))

    header <> body
  end

  def serialize(packets) when is_list(packets) do
    Enum.map_join(packets, &serialize/1)
  end

  @spec parse(binary()) :: {:ok, [t()]} | {:error, any()}
  def parse(packets) do
    do_parse(packets, [])
  end

  defp do_parse(<<>>, acc), do: {:ok, Enum.reverse(acc)}

  defp do_parse(<<raw_header::binary-size(4), body_and_rest::binary>>, acc) do
    with {:ok, %{header: header, body_size: length, padding?: padding?}} <-
           Header.parse(raw_header),
         <<body::binary-size(length), rest::binary>> <- body_and_rest,
         {:ok, {body, _padding}} <- RTP.Utils.strip_padding(body, padding?) do
      case parse_packet(body, header) do
        {:ok, packet} ->
          do_parse(rest, [packet | acc])

        {:error, :unknown_packet_type} ->
          Membrane.Logger.debug("""
          Ignoring rtcp packet with packet type #{header.packet_type}:
          #{inspect(raw_header <> body_and_rest, limit: :infinity)}
          Reason: :unknown_packet_type
          """)

          do_parse(rest, acc)

        error ->
          error
      end
    else
      {:error, reason} -> {:error, reason}
      _error -> {:error, :malformed_packet}
    end
  end

  defp do_parse(_binary, _acc), do: {:error, :malformed_packet}

  defp parse_packet(body, %Header{} = header) do
    case BiMap.fetch(@packet_type_module, header.packet_type) do
      {:ok, packet_module} ->
        packet_module.decode(body, header.packet_specific)

      :error ->
        {:error, :unknown_packet_type}
    end
  end
end
