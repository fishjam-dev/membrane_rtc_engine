defmodule ExSDP.Media do
  @moduledoc """
  This module represents the Media field of SDP.

  For more details please see [RFC4566 Section 5.14](https://tools.ietf.org/html/rfc4566#section-5.14)
  """
  use Bunch
  use Bunch.Access

  alias ExSDP.{
    Attribute,
    Bandwidth,
    ConnectionData,
    Encryption
  }

  alias ExSDP.Attribute.{
    Extmap,
    FMTP,
    Group,
    MSID,
    RTCPFeedback,
    RTPMapping,
    SSRC,
    SSRCGroup
  }

  @enforce_keys [:type, :port, :protocol, :fmt]
  defstruct @enforce_keys ++
              [
                :title,
                :encryption,
                port_count: 1,
                connection_data: [],
                bandwidth: [],
                attributes: []
              ]

  @type t :: %__MODULE__{
          type: type(),
          port: :inet.port_number(),
          port_count: non_neg_integer(),
          protocol: binary(),
          fmt: binary() | [0..127],
          title: binary() | nil,
          connection_data: [ConnectionData.t()],
          bandwidth: [Bandwidth.t()],
          encryption: Encryption.t() | nil,
          attributes: [Attribute.t()]
        }

  @typedoc """
  Represents type of media. In [RFC4566](https://tools.ietf.org/html/rfc4566#section-5.14)
  there are defined "audio", "video", "text", "application", and "message" types.

  Known types are represented as atoms others are binaries.
  """
  @type type :: :audio | :video | :text | :application | :message | binary()

  # For searching struct attributes by atoms
  @struct_attr_keys %{
    :rtpmap => RTPMapping,
    :msid => MSID,
    :fmtp => FMTP,
    :ssrc => SSRC,
    :ssrc_group => SSRCGroup,
    :rtcp_feedback => RTCPFeedback,
    :group => Group,
    :extmap => Extmap
  }

  @spec new(
          type :: type(),
          port :: :inet.port_number(),
          protocol :: binary(),
          fmt :: binary() | 0..127 | [0..127],
          opts :: [port_count: non_neg_integer()]
        ) :: t()
  def new(type, port, protocol, fmt, opts \\ []) do
    %__MODULE__{
      type: type,
      port: port,
      port_count: opts[:port_count] || 1,
      protocol: protocol,
      fmt: Bunch.listify(fmt)
    }
  end

  @spec add_attribute(media :: t(), attribute :: Attribute.t()) :: t()
  def add_attribute(media, attribute), do: add_attributes(media, [attribute])

  @spec add_attributes(media :: t(), attributes :: [Attribute.t()]) :: t()
  def add_attributes(media, attributes) when is_list(attributes),
    do: Map.update!(media, :attributes, &(&1 ++ attributes))

  @spec get_attribute(media :: t(), key :: module() | atom() | binary()) :: Attribute.t() | nil
  def get_attribute(media, key) do
    key = Map.get(@struct_attr_keys, key, key)

    media.attributes
    |> Enum.find(fn
      %module{} -> module == key
      {k, _v} -> k == key
      # for flag attributes
      k -> k == key
    end)
  end

  @spec get_attributes(media :: t(), key :: module() | atom() | binary()) :: [Attribute.t()]
  def get_attributes(media, key) do
    key = Map.get(@struct_attr_keys, key, key)

    media.attributes
    |> Enum.filter(fn
      %module{} -> module == key
      {k, _v} -> k == key
      # for flag attributes
      k -> k == key
    end)
  end

  @spec parse(binary()) :: {:ok, t()} | {:error, :invalid_media_spec | :malformed_port_number}
  def parse(media) do
    withl conn: [type, port, proto, fmt] <- String.split(media, " ", parts: 4),
          port: {port, port_count} when port in 0..65_535 <- Integer.parse(port),
          port_count: port_count when port_count > 0 <- parse_port_count(port_count),
          fmt: {:ok, fmt} <- parse_fmt(fmt, proto) do
      media = %__MODULE__{
        type: parse_type(type),
        port: port,
        port_count: port_count,
        protocol: proto,
        fmt: fmt
      }

      {:ok, media}
    else
      conn: _ -> {:error, :invalid_media_spec}
      port: _ -> {:error, :invalid_port_number}
      port_count: _ -> {:error, :invalid_port_count}
      fmt: error -> error
    end
  end

  @spec parse_optional([binary()], t()) :: {:ok, {[binary()], t()}} | {:error, atom()}
  def parse_optional(lines, media)

  def parse_optional([""], media), do: {:ok, {[""], finalize_optional_parsing(media)}}

  def parse_optional(["m=" <> _ | _] = lines, media),
    do: {:ok, {lines, finalize_optional_parsing(media)}}

  def parse_optional(["i=" <> title | rest], media),
    do: parse_optional(rest, %__MODULE__{media | title: title})

  def parse_optional(["c=" <> conn | rest], media) do
    with {:ok, %ConnectionData{} = connection_data} <- ConnectionData.parse(conn) do
      connection_data = media.connection_data ++ [connection_data]
      connection_data = %__MODULE__{media | connection_data: connection_data}
      parse_optional(rest, connection_data)
    end
  end

  def parse_optional(["b=" <> bandwidth | rest], %__MODULE__{bandwidth: acc_bandwidth} = media) do
    with {:ok, bandwidth} <- Bandwidth.parse(bandwidth) do
      bandwidth = %__MODULE__{media | bandwidth: [bandwidth | acc_bandwidth]}
      parse_optional(rest, bandwidth)
    end
  end

  def parse_optional(["k=" <> encryption | rest], media) do
    with {:ok, encryption} <- Encryption.parse(encryption) do
      encryption = %__MODULE__{media | encryption: encryption}
      parse_optional(rest, encryption)
    end
  end

  def parse_optional(["a=" <> attribute | rest], %__MODULE__{attributes: attrs} = media) do
    with {:ok, attribute} <- Attribute.parse(attribute, media_type: media.type) do
      media = %__MODULE__{media | attributes: [attribute | attrs]}
      parse_optional(rest, media)
    end
  end

  @spec apply_session(__MODULE__.t(), ExSDP.t()) :: __MODULE__.t()
  def apply_session(media, session) do
    session
    |> Map.from_struct()
    |> Enum.reduce(Map.from_struct(media), fn
      {inherited_key, value}, acc
      when inherited_key == :encryption ->
        if acc[inherited_key] != nil,
          do: acc,
          else: Map.put(acc, inherited_key, value)

      {inherited_key, value}, acc when inherited_key in [:connection_data, :bandwidth] ->
        if acc[inherited_key] != [],
          do: acc,
          else: Map.put(acc, inherited_key, value)

      _key_value, acc ->
        acc
    end)
    ~> struct(__MODULE__, &1)
  end

  defp finalize_optional_parsing(%__MODULE__{attributes: attrs} = media) do
    %__MODULE__{media | attributes: Enum.reverse(attrs)}
  end

  defp parse_type(type) when type in ["audio", "video", "text", "application", "message"],
    do: String.to_atom(type)

  defp parse_type(type) when is_binary(type), do: type

  defp parse_fmt(fmt, proto) when proto in ["RTP/AVP", "RTP/SAVP", "UDP/TLS/RTP/SAVPF"] do
    fmt
    |> String.split(" ")
    |> Bunch.Enum.try_map(fn single_fmt ->
      case Integer.parse(single_fmt) do
        {parsed_fmt, ""} -> {:ok, parsed_fmt}
        _invalid_fmt -> {:error, :invalid_fmt}
      end
    end)
  end

  defp parse_fmt(fmt, _proto), do: {:ok, fmt}

  defp parse_port_count(""), do: 1

  defp parse_port_count("/" <> port_count) do
    case Integer.parse(port_count) do
      {port_count, ""} -> port_count
      _not_matched -> :error
    end
  end
end

defimpl String.Chars, for: ExSDP.Media do
  @impl true
  def to_string(media) do
    import ExSDP.Sigil

    serialized_header = media |> header_fields |> String.trim()

    ~n"""
    #{serialized_header}
    #{ExSDP.Serializer.maybe_serialize("i", media.title)}
    #{ExSDP.Serializer.maybe_serialize("c", media.connection_data)}
    #{ExSDP.Serializer.maybe_serialize("b", media.bandwidth)}
    #{ExSDP.Serializer.maybe_serialize("k", media.encryption)}
    #{ExSDP.Serializer.maybe_serialize("a", media.attributes)}
    """
    # to_string has to return string without `\r\n` at the end
    |> String.trim()
  end

  defp header_fields(media) do
    """
    #{media.type} \
    #{serialize_port(media.port, media.port_count)} \
    #{media.protocol} \
    #{serialize_fmt(media.fmt)} \
    """
  end

  defp serialize_port(port, 1), do: "#{port}"
  defp serialize_port(port, port_count), do: "#{port}/#{port_count}"

  defp serialize_fmt(fmt) when is_binary(fmt), do: fmt
  defp serialize_fmt(fmt), do: Enum.map_join(fmt, " ", &Kernel.to_string/1)
end
