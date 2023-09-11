defmodule Membrane.RTP.VP8.PayloadDescriptor do
  @moduledoc """
  Defines a structure representing VP8 payload descriptor
  Described in details under following link: https://tools.ietf.org/html/rfc7741#section-4.2

         0 1 2 3 4 5 6 7                     0 1 2 3 4 5 6 7
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
        |X|R|N|S|R| PID | (REQUIRED)        |X|R|N|S|R| PID | (REQUIRED)
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
   X:   |I|L|T|K| RSV   | (OPTIONAL)   X:   |I|L|T|K| RSV   | (OPTIONAL)
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
   I:   |M| PictureID   | (OPTIONAL)   I:   |M| PictureID   | (OPTIONAL)
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
   L:   |   TL0PICIDX   | (OPTIONAL)        |   PictureID   |
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
   T/K: |TID|Y| KEYIDX  | (OPTIONAL)   L:   |   TL0PICIDX   | (OPTIONAL)
        +-+-+-+-+-+-+-+-+                   +-+-+-+-+-+-+-+-+
                                       T/K: |TID|Y| KEYIDX  | (OPTIONAL)
                                            +-+-+-+-+-+-+-+-+

  Structure with single octet picture ID on the left and structure with extended
  picture id on the right (when M bit is set)
  """

  @type first_octet :: binary()
  @type partition_index :: 0..7
  @type picture_id :: 0..32_767
  @type tl0picidx :: 0..255
  @type bit :: 0..1
  @type tid :: 0..3
  @type keyidx :: 0..31

  @type t :: %__MODULE__{
          x: bit(),
          n: bit(),
          s: bit(),
          partition_index: partition_index(),
          i: bit(),
          l: bit(),
          t: bit(),
          k: bit(),
          picture_id: picture_id(),
          tl0picidx: tl0picidx(),
          tid: tid(),
          y: bit(),
          keyidx: keyidx()
        }

  @enforce_keys [:x, :n, :s, :partition_index]
  defstruct @enforce_keys ++
              [
                i: 0,
                l: 0,
                t: 0,
                k: 0,
                m: 0,
                picture_id: 0,
                tl0picidx: 0,
                tid: 0,
                y: 0,
                keyidx: 0
              ]

  @spec serialize(__MODULE__.t()) :: binary()
  def serialize(payload_descriptor) do
    %__MODULE__{
      x: x,
      n: n,
      s: s,
      partition_index: partition_index,
      i: i,
      l: l,
      t: t,
      k: k,
      m: m,
      picture_id: picture_id,
      tl0picidx: tl0picidx,
      tid: tid,
      y: y,
      keyidx: keyidx
    } = payload_descriptor

    xnspid = <<x::1, 0::1, n::1, s::1, 0::1, partition_index::3>>

    iltk = if x == 1, do: <<i::1, l::1, t::1, k::1, 0::4>>, else: <<>>

    picture_id =
      case {i, m} do
        {1, 0} -> <<m::1, picture_id::7>>
        {1, 1} -> <<m::1, picture_id::15>>
        {0, _} -> <<>>
      end

    tl0picidx = if l == 1, do: <<tl0picidx>>, else: <<>>

    tidykeyidx = if t == 1 or k == 1, do: <<tid::2, y::1, keyidx::5>>, else: <<>>

    xnspid <> iltk <> picture_id <> tl0picidx <> tidykeyidx
  end

  @spec parse_payload_descriptor(binary()) ::
          {:error, :malformed_data | :payload_too_short} | {:ok, {t(), binary()}}
  def parse_payload_descriptor(payload) when byte_size(payload) <= 1,
    do: {:error, :payload_too_short}

  def parse_payload_descriptor(
        <<x::1, 0::1, n::1, s::1, 0::1, partition_index::3, rest::binary()>>
      )
      when byte_size(rest) > 0 do
    descriptor_acc = %__MODULE__{x: x, n: n, s: s, partition_index: partition_index}

    with {:ok, {descriptor_acc, rest}} <- get_extended_control_bits(descriptor_acc, rest),
         {:ok, {descriptor_acc, rest}} <- get_picture_id(descriptor_acc, rest),
         {:ok, {descriptor_acc, rest}} <- get_temporal_level_zero_index(descriptor_acc, rest),
         {:ok, {descriptor_acc, rest}} <- get_tidykeyidx(descriptor_acc, rest) do
      {:ok, {descriptor_acc, rest}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def parse_payload_descriptor(_payload), do: {:error, :malformed_data}

  defp get_extended_control_bits(%__MODULE__{x: 0} = descriptor_acc, rest),
    do: {:ok, {descriptor_acc, rest}}

  defp get_extended_control_bits(_descriptor_acc, <<_iltk::4, rsv::4, _rest::binary()>>)
       when rsv > 0,
       do: {:error, :malformed_data}

  defp get_extended_control_bits(
         descriptor_acc,
         <<extended_control_bits::binary-size(1), rest::binary>>
       )
       when byte_size(rest) > 0 do
    <<i::1, l::1, t::1, k::1, _rsv::4>> = extended_control_bits

    {:ok, {%__MODULE__{descriptor_acc | i: i, l: l, t: t, k: k}, rest}}
  end

  defp get_extended_control_bits(_descriptor_acc, _rest), do: {:error, :payload_too_short}

  defp get_picture_id(%__MODULE__{i: 0} = descriptor_acc, rest), do: {:ok, {descriptor_acc, rest}}

  defp get_picture_id(descriptor_acc, <<0::1, picture_id::7, rest::binary()>>)
       when byte_size(rest) > 0,
       do: {:ok, {%__MODULE__{descriptor_acc | m: 0, picture_id: picture_id}, rest}}

  defp get_picture_id(descriptor_acc, <<1::1, picture_id::15, rest::binary()>>)
       when byte_size(rest) > 0,
       do: {:ok, {%__MODULE__{descriptor_acc | m: 1, picture_id: picture_id}, rest}}

  defp get_picture_id(_descriptor_acc, _rest), do: {:error, :payload_too_short}

  defp get_temporal_level_zero_index(%__MODULE__{l: 0} = descriptor_acc, rest),
    do: {:ok, {descriptor_acc, rest}}

  defp get_temporal_level_zero_index(descriptor_acc, <<tl0picidx, rest::binary()>>)
       when byte_size(rest) > 0,
       do: {:ok, {%__MODULE__{descriptor_acc | tl0picidx: tl0picidx}, rest}}

  defp get_tidykeyidx(%__MODULE__{t: t, k: k} = descriptor_acc, rest) when t == 1 or k == 1 do
    case byte_size(rest) do
      1 ->
        {:error, :payload_to_short}

      _greater_than_one ->
        <<tid::2, y::1, keyidx::5, rest::binary()>> = rest
        {:ok, {%__MODULE__{descriptor_acc | tid: tid, y: y, keyidx: keyidx}, rest}}
    end
  end

  defp get_tidykeyidx(descriptor_acc, rest), do: {:ok, {descriptor_acc, rest}}
end
