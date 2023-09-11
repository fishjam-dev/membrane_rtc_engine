defmodule Membrane.RTCP.SdesPacket do
  @moduledoc """
  Parses Source Description (SDES) RTCP Packets defined in
  [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.5)
  """

  @behaviour Membrane.RTCP.Packet

  defmodule Chunk do
    @moduledoc false

    @type t :: %__MODULE__{
            cname: String.t() | nil,
            name: String.t() | nil,
            email: String.t() | nil,
            phone: String.t() | nil,
            loc: String.t() | nil,
            tool: String.t() | nil,
            note: String.t() | nil,
            priv: {String.t(), String.t()} | nil
          }
    defstruct [
      :cname,
      :name,
      :email,
      :phone,
      :loc,
      :tool,
      :note,
      :priv
    ]
  end

  defstruct chunks: []

  @type t() :: %__MODULE__{
          chunks: %{
            required(Membrane.RTP.ssrc_t()) => Keyword.t()
          }
        }

  @sdes_type_atom %{
    1 => :cname,
    2 => :name,
    3 => :email,
    4 => :phone,
    5 => :loc,
    6 => :tool,
    7 => :note,
    8 => :priv
  }

  @sdes_type_from_atom %{
    :cname => 1,
    :name => 2,
    :email => 3,
    :phone => 4,
    :loc => 5,
    :tool => 6,
    :note => 7,
    :priv => 8
  }

  @impl true
  def decode(packet, ssrc_count) do
    with {:ok, chunks} <- parse_chunks(packet, []),
         true <- ssrc_count == length(chunks) do
      chunks =
        chunks |> Enum.into(%{}, fn {ssrc, items} -> {ssrc, struct(__MODULE__.Chunk, items)} end)

      {:ok, %__MODULE__{chunks: chunks}}
    else
      {:error, _reason} = err -> err
      false -> {:error, :invalid_ssrc_count}
    end
  end

  defp parse_chunks(<<>>, acc), do: {:ok, acc}

  defp parse_chunks(<<ssrc::32, rest::binary>>, acc) do
    with {:ok, items, rest} <- parse_items(rest) do
      parse_chunks(rest, [{ssrc, items} | acc])
    end
  end

  defp parse_items(sdes_item, acc \\ [])

  # null item denoting end of the list
  defp parse_items(<<0::8, rest::binary>>, acc) do
    # skip padding unitil next 32-bit boundary
    to_skip = rest |> bit_size |> rem(32)
    <<_skipped::size(to_skip), next_chunk::binary>> = rest
    {:ok, acc, next_chunk}
  end

  # PRIV item
  defp parse_items(<<8::8, length::8, content::binary-size(length), rest::binary>>, acc) do
    <<prefix_len::8, prefix::binary-size(prefix_len), value>> = content
    item = {@sdes_type_atom[8], {prefix, value}}
    parse_items(rest, [item | acc])
  end

  defp parse_items(<<si_type::8, length::8, value::binary-size(length), rest::binary>>, acc)
       when si_type in 0..7 do
    item = {@sdes_type_atom[si_type], value}
    parse_items(rest, [item | acc])
  end

  defp parse_items(<<_si_type::8, _payload::binary>>, _acc) do
    {:error, :unknown_si_type}
  end

  @impl true
  def encode(%__MODULE__{chunks: chunks}) do
    chunk_list = chunks |> Enum.to_list()

    body =
      chunk_list
      |> Enum.map_join(fn {ssrc, chunk} ->
        <<ssrc::32, encode_chunk(chunk)::binary>>
      end)

    {body, length(chunk_list)}
  end

  @spec encode_chunk(chunk :: Chunk.t()) :: binary()
  defp encode_chunk(chunk) do
    body =
      chunk
      |> Map.from_struct()
      |> Enum.map_join(fn
        {_key, nil} ->
          <<>>

        {:priv, {prefix, value}} ->
          prefix_len = byte_size(prefix)
          total_len = 1 + prefix_len + byte_size(value)
          <<8::8, total_len::8, prefix_len::8, prefix::binary, value::binary>>

        {key, value} ->
          <<@sdes_type_from_atom[key]::8, byte_size(value)::8, value::binary>>
      end)

    pad_bits = 32 - (body |> bit_size() |> rem(32))
    end_marker = <<0::size(pad_bits)>>

    body <> end_marker
  end
end
