defmodule Membrane.RTP.Packet do
  @moduledoc """
  Defines a struct describing an RTP packet and a way to parse and serialize it.
  Based on [RFC3550](https://tools.ietf.org/html/rfc3550#page-13)

  Supports only one-byte header from [RFC8285](https://datatracker.ietf.org/doc/html/rfc8285#section-4.2),
  as according to the document this form is preferred and it must be supported by all receivers.
  """

  alias Membrane.RTP.{Header, Utils}

  @type t :: %__MODULE__{
          header: Header.t(),
          payload: binary()
        }

  @typedoc """
  Possible padding size.

  It includes the last byte denoting the size of the padding.
  """
  @type padding_size :: 0..255

  @enforce_keys [:header, :payload]
  defstruct @enforce_keys

  # 0xBEDE for Venerable Bede from RFC 8285
  @one_byte_header_identifier <<0xBE, 0xDE>>

  @spec identify(binary()) :: :rtp | :rtcp
  def identify(<<_first_byte, _marker::1, payload_type::7, _rest::binary>>)
      when payload_type in 64..95,
      do: :rtcp

  def identify(_packet), do: :rtp

  @spec serialize(t, padding_size: padding_size()) :: binary
  def serialize(%__MODULE__{} = packet, opts \\ []) do
    %__MODULE__{header: header, payload: payload} = packet
    %Header{version: 2} = header
    padding_size = Keyword.get(opts, :padding_size, 0)
    has_padding = if padding_size > 0, do: 1, else: 0
    has_extension = if header.extensions == [], do: 0, else: 1
    marker = if header.marker, do: 1, else: 0
    csrcs = Enum.map_join(header.csrcs, &<<&1::32>>)
    padding = Utils.generate_padding(padding_size)

    <<header.version::2, has_padding::1, has_extension::1, length(header.csrcs)::4, marker::1,
      header.payload_type::7, header.sequence_number::16, header.timestamp::32, header.ssrc::32,
      csrcs::binary, serialize_header_extensions(header.extensions)::binary, payload::binary,
      padding::binary>>
  end

  defp serialize_header_extensions([]), do: <<>>

  defp serialize_header_extensions(extensions) do
    extensions =
      Enum.reduce(extensions, <<>>, fn extension, acc ->
        acc <> serialize_header_extension(extension)
      end)

    extensions_size = byte_size(extensions)
    padding = calculate_padding_size(extensions_size) * 8

    extension_header_size =
      div(extensions_size, 4) + if rem(extensions_size, 4) == 0, do: 0, else: 1

    <<@one_byte_header_identifier, extension_header_size::16,
      extensions::binary-size(extensions_size), 0::integer-size(padding)>>
  end

  defp calculate_padding_size(extensions_size) do
    how_many_bytes_over_32 = rem(extensions_size, 4)
    if how_many_bytes_over_32 == 0, do: 0, else: 4 - how_many_bytes_over_32
  end

  defp serialize_header_extension(nil) do
    <<>>
  end

  defp serialize_header_extension(%Header.Extension{data: data} = extension) do
    data_size = byte_size(data) - 1
    <<extension.identifier::integer-size(4), data_size::integer-size(4), data::binary>>
  end

  @spec parse(binary(), boolean()) ::
          {:ok,
           %{packet: t(), padding_size: padding_size(), total_header_size: non_neg_integer()}}
          | {:error, :wrong_version | :malformed_packet}
  def parse(packet, encrypted?)

  def parse(<<version::2, _payload::bitstring>>, _encrypted?) when version != 2,
    do: {:error, :wrong_version}

  def parse(
        <<version::2, has_padding::1, has_extension::1, csrcs_cnt::4, marker::1, payload_type::7,
          sequence_number::16, timestamp::32, ssrc::32, csrcs::binary-size(csrcs_cnt)-unit(32),
          rest::binary>> = original_packet,
        encrypted?
      ) do
    with {:ok, {extensions, payload}} <-
           parse_header_extension(rest, has_extension == 1),
         {:ok, {payload, padding}} <-
           Utils.strip_padding(payload, not encrypted? and has_padding == 1) do
      header = %Header{
        version: version,
        marker: marker == 1,
        ssrc: ssrc,
        sequence_number: sequence_number,
        payload_type: payload_type,
        timestamp: timestamp,
        csrcs: for(<<csrc::32 <- csrcs>>, do: csrc),
        extensions: extensions
      }

      {:ok,
       %{
         packet: %__MODULE__{
           header: header,
           payload: if(encrypted?, do: original_packet, else: payload)
         },
         padding_size: padding,
         total_header_size: byte_size(original_packet) - byte_size(payload) - padding
       }}
    else
      :error -> {:error, :malformed_packet}
    end
  end

  def parse(_binary, _parse_payload?), do: {:error, :malformed_packet}

  defp parse_header_extension(binary, header_present?)
  defp parse_header_extension(binary, false), do: {:ok, {[], binary}}

  defp parse_header_extension(
         <<_profile_specific::binary-size(2), data_len::16, data::binary-size(data_len)-unit(32),
           rest::binary>>,
         true
       ) do
    extensions = parse_extension_data(data, [])
    extensions = Enum.reverse(extensions)
    {:ok, {extensions, rest}}
  end

  defp parse_header_extension(_binary, true), do: :error

  defp parse_extension_data(<<>>, acc), do: acc

  defp parse_extension_data(data, acc) do
    case parse_one_byte_header(data) do
      {data, rest} -> parse_extension_data(rest, [data | acc])
      rest -> parse_extension_data(rest, acc)
    end
  end

  # According to RFC8285:
  #   - ID=0 and length=0 is a padding byte
  #   - ID=0 and length>0 must terminate further parsing
  #   - ID=15 must terminate further parsing
  defp parse_one_byte_header(<<0::4, 0::4, extensions::binary>>),
    do: extensions

  defp parse_one_byte_header(<<0::4, _len_data::4, _rest::binary>>),
    do: <<>>

  defp parse_one_byte_header(<<15::4, _len_data::4, _rest::binary>>),
    do: <<>>

  defp parse_one_byte_header(
         <<local_identifier::integer-size(4), len_data::integer-size(4), extensions::binary>>
       ) do
    len_data = len_data + 1
    <<data::binary-size(len_data), next_extensions::binary>> = extensions
    extension = %Header.Extension{identifier: local_identifier, data: data}
    {extension, next_extensions}
  end
end
