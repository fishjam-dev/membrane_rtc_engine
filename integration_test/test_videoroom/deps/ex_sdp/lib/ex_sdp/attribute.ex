defmodule ExSDP.Attribute do
  @moduledoc """
  This module represents Attributes fields of SDP.
  """
  use Bunch.Access

  alias __MODULE__.{Extmap, FMTP, Group, MSID, RTCPFeedback, RTPMapping, SSRC, SSRCGroup}

  @type hash_function :: :sha1 | :sha224 | :sha256 | :sha384 | :sha512
  @type setup_value :: :active | :passive | :actpass | :holdconn
  @type orient_value :: :portrait | :landscape | :seascape
  @type type_value :: :broadcast | :meeting | :moderated | :test | :H332
  @type framerate_value :: float() | {integer(), integer()}

  @flag_attributes [
    :recvonly,
    :sendrecv,
    :sendonly,
    :inactive,
    :extmap_allow_mixed,
    :rtcp_mux,
    :rtcp_rsize
  ]
  @type flag_attributes :: unquote(Bunch.Typespec.enum_to_alternative(@flag_attributes))

  @type cat :: {:cat, binary()}
  @type charset :: {:charset, binary()}
  @type keywds :: {:keywds, binary()}
  @type orient :: {:orient, orient_value()}
  @type lang :: {:lang, binary()}
  @type sdplang :: {:sdplang, binary()}
  @type tool :: {:tool, binary()}
  @type type :: {:type, type_value()}
  @type framerate :: {:framerate, framerate_value()}
  @type maxptime :: {:maxptime, non_neg_integer()}
  @type ptime :: {:ptime, non_neg_integer()}
  @type quality :: {:quality, non_neg_integer()}
  @type ice_ufrag :: {:ice_ufrag, binary()}
  @type ice_pwd :: {:ice_pwd, binary()}
  @type ice_options :: {:ice_options, binary() | [binary()]}
  @type fingerprint :: {:fingerprint, {hash_function(), binary()}}
  @type setup :: {:setup, setup_value()}
  @type mid :: {:mid, binary()}

  @type t ::
          Extmap.t()
          | FMTP.t()
          | Group.t()
          | MSID.t()
          | RTCPFeedback.t()
          | RTPMapping.t()
          | SSRC.t()
          | SSRCGroup.t()
          | cat()
          | charset()
          | keywds()
          | orient()
          | lang()
          | sdplang()
          | tool()
          | type()
          | framerate()
          | maxptime()
          | ptime()
          | quality()
          | ice_ufrag()
          | ice_pwd()
          | ice_options()
          | fingerprint()
          | setup()
          | mid()
          | flag_attributes()
          | {String.t(), String.t()}
          | String.t()

  @flag_attributes_strings @flag_attributes |> Enum.map(&to_string/1)

  @doc """
  Parses SDP Attribute line.

  `line` is a string in form of `a=attribute` or `a=attribute:value`.
  `opts` is a keyword list that can contain some information for parsers.

  Unknown attributes keys are returned as strings, known ones as atoms.
  For known attributes please refer to `t()`.
  """
  @spec parse(binary(), opts :: Keyword.t()) :: {:ok, t()} | {:error, atom()}
  def parse(line, opts \\ []) do
    [attribute | value] = String.split(line, ":", parts: 2)
    do_parse(attribute, List.first(value), opts)
  end

  defp do_parse("rtpmap", value, opts), do: RTPMapping.parse(value, opts)
  defp do_parse("msid", value, _opts), do: MSID.parse(value)
  defp do_parse("fmtp", value, _opts), do: FMTP.parse(value)
  defp do_parse("ssrc-group", value, _opts), do: SSRCGroup.parse(value)
  defp do_parse("ssrc", value, _opts), do: SSRC.parse(value)
  defp do_parse("group", value, _opts), do: Group.parse(value)
  defp do_parse("extmap", value, _opts), do: Extmap.parse(value)
  defp do_parse("rtcp-fb", value, _opts), do: RTCPFeedback.parse(value)
  # Flag allowing to mix one- and two-byte header extensions
  defp do_parse("extmap-allow-mixed", nil, _opts), do: {:ok, :extmap_allow_mixed}
  defp do_parse("cat", value, _opts), do: {:ok, {:cat, value}}
  defp do_parse("charset", value, _opts), do: {:ok, {:charset, value}}
  defp do_parse("keywds", value, _opts), do: {:ok, {:keywds, value}}
  defp do_parse("orient", value, _opts), do: parse_orient(value)
  defp do_parse("lang", value, _opts), do: {:ok, {:lang, value}}
  defp do_parse("sdplang", value, _opts), do: {:ok, {:sdplang, value}}
  defp do_parse("tool", value, _opts), do: {:ok, {:tool, value}}
  defp do_parse("type", value, _opts), do: parse_type(value)
  defp do_parse("framerate", value, _opts), do: parse_framerate(value)
  defp do_parse("ice-lite", _value, _opts), do: {:ok, :ice_lite}
  defp do_parse("ice-ufrag", value, _opts), do: {:ok, {:ice_ufrag, value}}
  defp do_parse("ice-pwd", value, _opts), do: {:ok, {:ice_pwd, value}}
  defp do_parse("ice-options", value, _opts), do: {:ok, {:ice_options, String.split(value, " ")}}
  defp do_parse("fingerprint", value, _opts), do: parse_fingerprint(value)
  defp do_parse("setup", value, _opts), do: parse_setup(value)
  defp do_parse("mid", value, _opts), do: {:ok, {:mid, value}}
  defp do_parse("rtcp-mux", _value, _opts), do: {:ok, :rtcp_mux}
  defp do_parse("rtcp-rsize", _value, _opts), do: {:ok, :rtcp_rsize}

  defp do_parse(attribute, value, _opts) when attribute in ["maxptime", "ptime", "quality"] do
    case Integer.parse(value) do
      {number, ""} -> {:ok, {String.to_atom(attribute), number}}
      _invalid_attribute -> {:error, :invalid_attribute}
    end
  end

  defp do_parse(flag, nil, _opts) when flag in @flag_attributes_strings,
    do: {:ok, String.to_atom(flag)}

  defp do_parse(flag, nil, _opts), do: {:ok, flag}

  defp do_parse(attribute, value, _opts), do: {:ok, {attribute, value}}

  defp parse_orient(orient) do
    case orient do
      string when string in ["portrait", "landscape", "seascape"] ->
        {:ok, {:orient, String.to_atom(string)}}

      _invalid_orient ->
        {:error, :invalid_orient}
    end
  end

  defp parse_type(type) do
    case type do
      string when string in ["broadcast", "meeting", "moderated", "test", "H332"] ->
        {:ok, {:type, String.to_atom(string)}}

      _invalid_type ->
        {:error, :invalid_type}
    end
  end

  defp parse_framerate(framerate) do
    case String.split(framerate, "/") do
      [value] -> {:ok, {:framerate, String.to_float(value)}}
      [left, right] -> {:ok, {:framerate, {String.to_integer(left), String.to_integer(right)}}}
      _invalid_framerate -> {:error, :invalid_framerate}
    end
  end

  defp parse_fingerprint(fingerprint) do
    case String.split(fingerprint, " ") do
      ["sha-1", value] -> {:ok, {:fingerprint, {:sha1, value}}}
      ["sha-224", value] -> {:ok, {:fingerprint, {:sha224, value}}}
      ["sha-256", value] -> {:ok, {:fingerprint, {:sha256, value}}}
      ["sha-384", value] -> {:ok, {:fingerprint, {:sha384, value}}}
      ["sha-512", value] -> {:ok, {:fingerprint, {:sha512, value}}}
      _invalid_fingerprint -> {:error, :invalid_fingerprint}
    end
  end

  defp parse_setup(setup) do
    case setup do
      "active" -> {:ok, {:setup, :active}}
      "passive" -> {:ok, {:setup, :passive}}
      "actpass" -> {:ok, {:setup, :actpass}}
      "holdconn" -> {:ok, {:setup, :holdconn}}
      _invalid_setup -> {:error, :invalid_setup}
    end
  end
end
