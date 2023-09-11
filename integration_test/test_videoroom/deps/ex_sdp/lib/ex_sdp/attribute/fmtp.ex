defmodule ExSDP.Attribute.FMTP do
  @moduledoc """
  This module represents fmtp (RFC 5576).

  Parameters for:

  * H264 (not all, RFC 6184),
  * VP8, VP9, OPUS (RFC 7587)
  * RTX (RFC 4588)
  * FLEXFEC (RFC 8627)
  * Telephone Events (RFC 4733)
  * RED (RFC 2198)

  are currently supported.
  """

  alias ExSDP.Attribute.RTPMapping
  alias ExSDP.Utils

  @enforce_keys [:pt]
  defstruct @enforce_keys ++
              [
                # H264
                :profile_level_id,
                :level_asymmetry_allowed,
                :packetization_mode,
                :max_mbps,
                :max_smbps,
                :max_fs,
                :max_dpb,
                :max_br,
                :sprop_parameter_sets,
                # OPUS
                :maxaveragebitrate,
                :maxplaybackrate,
                :sprop_maxcapturerate,
                :maxptime,
                :ptime,
                :minptime,
                :stereo,
                :cbr,
                :useinbandfec,
                :usedtx,
                # VP8/9
                :profile_id,
                :max_fr,
                # RTX
                :apt,
                :rtx_time,
                # FLEXFEC
                :repair_window,
                # Telephone Events
                :dtmf_tones,
                # RED
                :redundant_payloads,
                unknown: []
              ]

  @type t :: %__MODULE__{
          profile_level_id: non_neg_integer() | nil,
          max_mbps: non_neg_integer() | nil,
          max_smbps: non_neg_integer() | nil,
          max_fs: non_neg_integer() | nil,
          max_dpb: non_neg_integer() | nil,
          max_br: non_neg_integer() | nil,
          level_asymmetry_allowed: boolean() | nil,
          packetization_mode: non_neg_integer() | nil,
          sprop_parameter_sets: %{sps: binary(), pps: binary()} | nil,
          # OPUS
          maxaveragebitrate: non_neg_integer() | nil,
          maxplaybackrate: non_neg_integer() | nil,
          sprop_maxcapturerate: non_neg_integer() | nil,
          maxptime: non_neg_integer() | nil,
          ptime: non_neg_integer() | nil,
          minptime: non_neg_integer() | nil,
          stereo: boolean() | nil,
          cbr: boolean() | nil,
          useinbandfec: boolean() | nil,
          usedtx: boolean() | nil,
          # VP8/9
          profile_id: non_neg_integer() | nil,
          max_fr: non_neg_integer() | nil,
          # RTX
          apt: RTPMapping.payload_type_t() | nil,
          rtx_time: non_neg_integer() | nil,
          # FLEXFEC
          repair_window: non_neg_integer() | nil,
          # Telephone Events
          dtmf_tones: String.t() | nil,
          # RED
          redundant_payloads: [RTPMapping.payload_type_t()] | nil,
          # params that are currently not supported
          unknown: [String.t()]
        }

  @typedoc """
  Key that can be used for searching this attribute using `ExSDP.Media.get_attribute/2`.
  """
  @type attr_key :: :fmtp

  @typedoc """
  Reason of parsing failure.
  """
  @type reason ::
          :invalid_fmtp
          | :invalid_pt
          | :invalid_sprop_parameter_sets
          | :string_nan
          | :string_not_hex
          | :string_not_0_nor_1

  @spec parse(binary()) :: {:ok, t()} | {:error, reason()}
  def parse(fmtp) do
    with [pt_string, rest] <- String.split(fmtp, " ", parts: 2),
         {:ok, pt} <- Utils.parse_payload_type(pt_string) do
      rest
      |> String.split(";")
      # remove leading whitespaces
      |> Enum.map(&String.trim(&1))
      |> do_parse(%__MODULE__{pt: pt})
    else
      {:error, _reason} = err -> err
      _other -> :invalid_fmtp
    end
  end

  defp do_parse([], fmtp), do: {:ok, fmtp}

  defp do_parse(params, fmtp) do
    case parse_param(params, fmtp) do
      {rest, %__MODULE__{} = fmtp} -> do_parse(rest, fmtp)
      {:error, _reason} = error -> error
    end
  end

  defp parse_param(["profile-level-id=" <> profile_level_id | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_hex_string(profile_level_id),
         do: {rest, %{fmtp | profile_level_id: value}}
  end

  defp parse_param(["level-asymmetry-allowed=" <> level_asymmetry_allowed | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_bool_string(level_asymmetry_allowed),
         do: {rest, %{fmtp | level_asymmetry_allowed: value}}
  end

  defp parse_param(["packetization-mode=" <> packetization_mode | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(packetization_mode),
         do: {rest, %{fmtp | packetization_mode: value}}
  end

  defp parse_param(["sprop-parameter-sets=" <> sprop_parameter_sets | rest], fmtp) do
    with {:ok, value} <- Utils.parse_sprop_parameter_sets(sprop_parameter_sets),
         do: {rest, %{fmtp | sprop_parameter_sets: value}}
  end

  defp parse_param(["max-mbps=" <> max_mbps | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_mbps),
         do: {rest, %{fmtp | max_mbps: value}}
  end

  defp parse_param(["max-smbps=" <> max_smbps | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_smbps),
         do: {rest, %{fmtp | max_smbps: value}}
  end

  defp parse_param(["max-fs=" <> max_fs | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_fs), do: {rest, %{fmtp | max_fs: value}}
  end

  defp parse_param(["max-dpb=" <> max_dpb | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_dpb),
         do: {rest, %{fmtp | max_dpb: value}}
  end

  defp parse_param(["max-br=" <> max_br | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_br), do: {rest, %{fmtp | max_br: value}}
  end

  defp parse_param(["maxaveragebitrate=" <> maxaveragebitrate | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(maxaveragebitrate),
         do: {rest, %{fmtp | maxaveragebitrate: value}}
  end

  defp parse_param(["maxplaybackrate=" <> maxplaybackrate | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(maxplaybackrate),
         do: {rest, %{fmtp | maxplaybackrate: value}}
  end

  defp parse_param(["sprop-maxcapturerate=" <> sprop_maxcapturerate | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(sprop_maxcapturerate),
         do: {rest, %{fmtp | sprop_maxcapturerate: value}}
  end

  defp parse_param(["maxptime=" <> maxptime | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(maxptime),
         do: {rest, %{fmtp | maxptime: value}}
  end

  defp parse_param(["ptime=" <> ptime | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(ptime),
         do: {rest, %{fmtp | ptime: value}}
  end

  defp parse_param(["minptime=" <> minptime | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(minptime),
         do: {rest, %{fmtp | minptime: value}}
  end

  defp parse_param(["stereo=" <> stereo | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_bool_string(stereo),
         do: {rest, %{fmtp | stereo: value}}
  end

  defp parse_param(["cbr=" <> cbr | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_bool_string(cbr),
         do: {rest, %{fmtp | cbr: value}}
  end

  defp parse_param(["useinbandfec=" <> useinbandfec | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_bool_string(useinbandfec),
         do: {rest, %{fmtp | useinbandfec: value}}
  end

  defp parse_param(["usedtx=" <> usedtx | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_bool_string(usedtx),
         do: {rest, %{fmtp | usedtx: value}}
  end

  defp parse_param(["max-fr=" <> max_fr | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(max_fr),
         do: {rest, %{fmtp | max_fr: value}}
  end

  defp parse_param(["profile-id=" <> profile_id | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(profile_id),
         do: {rest, %{fmtp | profile_id: value}}
  end

  defp parse_param(["apt=" <> value | rest], fmtp) do
    with {:ok, value} <- Utils.parse_payload_type(value), do: {rest, %{fmtp | apt: value}}
  end

  defp parse_param(["rtx-time=" <> value | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(value), do: {rest, %{fmtp | rtx_time: value}}
  end

  defp parse_param(["repair-window=" <> value | rest], fmtp) do
    with {:ok, value} <- Utils.parse_numeric_string(value),
         do: {rest, %{fmtp | repair_window: value}}
  end

  defp parse_param([head | rest] = params, fmtp) do
    # this is for non-key-value parameters as `key=value` format is not mandatory
    cond do
      String.contains?(head, "=") -> {rest, Map.update!(fmtp, :unknown, &(&1 ++ [head]))}
      String.contains?(head, "/") -> parse_redundant_payloads_param(params, fmtp)
      true -> parse_dtmf_tones_param(params, fmtp)
    end
  end

  defp parse_dtmf_tones_param([head | rest], fmtp) do
    with dtmf_tones <- String.split(head, ","),
         true <- validate_dtmf_tones(dtmf_tones) do
      {rest, Map.put(fmtp, :dtmf_tones, head)}
    else
      _error -> {:error, :invalid_dtmf_tones}
    end
  end

  defp parse_redundant_payloads_param([head | rest], fmtp) do
    with redundant_payloads <- String.split(head, "/"),
         {:ok, redundant_payloads} <-
           Bunch.Enum.try_map(redundant_payloads, &Utils.parse_payload_type/1) do
      # We need uniq because Chrome sends 111/111 most likely to avoid confusion with dtmf_tones_param
      {rest, Map.put(fmtp, :redundant_payloads, Enum.uniq(redundant_payloads))}
    end
  end

  defp validate_dtmf_tones(dtmf_tones) do
    Enum.all?(dtmf_tones, &validate_dtmf_tone(&1))
  end

  defp validate_dtmf_tone(dtmf_tone) do
    case String.split(dtmf_tone, "-") do
      [start_range, end_range] ->
        with {:ok, start_range} <- Utils.parse_numeric_string(start_range),
             {:ok, end_range} <- Utils.parse_numeric_string(end_range) do
          start_range < end_range and 0 <= start_range and end_range <= 255
        else
          _error -> false
        end

      [single_tone] ->
        case Utils.parse_numeric_string(single_tone) do
          {:ok, single_tone} -> 0 <= single_tone and single_tone <= 255
          _other -> false
        end

      _other ->
        false
    end
  end
end

defimpl String.Chars, for: ExSDP.Attribute.FMTP do
  @impl true
  def to_string(fmtp) do
    alias ExSDP.Serializer

    params =
      [
        # H264
        Serializer.maybe_serialize_hex("profile-level-id", fmtp.profile_level_id),
        Serializer.maybe_serialize("max-mbps", fmtp.max_mbps),
        Serializer.maybe_serialize("max-smbps", fmtp.max_smbps),
        Serializer.maybe_serialize("max-fs", fmtp.max_fs),
        Serializer.maybe_serialize("max-dpb", fmtp.max_dpb),
        Serializer.maybe_serialize("max-br", fmtp.max_br),
        Serializer.maybe_serialize("level-asymmetry-allowed", fmtp.level_asymmetry_allowed),
        Serializer.maybe_serialize("packetization-mode", fmtp.packetization_mode),
        Serializer.maybe_serialize("sprop-parameter-sets", fmtp.sprop_parameter_sets),
        # OPUS
        Serializer.maybe_serialize("maxaveragebitrate", fmtp.maxaveragebitrate),
        Serializer.maybe_serialize("maxplaybackrate", fmtp.maxplaybackrate),
        Serializer.maybe_serialize("sprop_maxcapturerate", fmtp.sprop_maxcapturerate),
        Serializer.maybe_serialize("maxptime", fmtp.maxptime),
        Serializer.maybe_serialize("ptime", fmtp.ptime),
        Serializer.maybe_serialize("minptime", fmtp.minptime),
        Serializer.maybe_serialize("stereo", fmtp.stereo),
        Serializer.maybe_serialize("cbr", fmtp.cbr),
        Serializer.maybe_serialize("useinbandfec", fmtp.useinbandfec),
        Serializer.maybe_serialize("usedtx", fmtp.usedtx),
        # VP8/9
        Serializer.maybe_serialize("profile-id", fmtp.profile_id),
        Serializer.maybe_serialize("max-fr", fmtp.max_fr),
        # RTX
        Serializer.maybe_serialize("apt", fmtp.apt),
        Serializer.maybe_serialize("rtx-time", fmtp.rtx_time),
        # FLEXFEC
        Serializer.maybe_serialize("repair-window", fmtp.repair_window),
        # Telephone Events
        Serializer.maybe_serialize("dtmf-tones", fmtp.dtmf_tones),
        # RED
        Serializer.maybe_serialize_list(fmtp.redundant_payloads, "/")
      ]
      |> Enum.filter(fn param -> param != "" end)
      |> Enum.join(";")

    "fmtp:#{fmtp.pt} #{params}"
  end
end
