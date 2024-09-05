defmodule Membrane.WebRTC.Track do
  @moduledoc """
  Module representing a WebRTC track.
  """
  require Membrane.Logger

  alias Membrane.RTP
  alias ExSDP.Attribute.{Extmap, FMTP, RTPMapping}
  alias Membrane.WebRTC.Extension

  @supported_rids ["l", "m", "h"]

  @enforce_keys [
    :type,
    :stream_id,
    :id,
    :name,
    :mid,
    :rids,
    :status,
    :extmaps
  ]
  defstruct @enforce_keys ++
              [
                ssrc: nil,
                rtx_ssrc: nil,
                selected_encoding_key: nil,
                selected_encoding: nil,
                offered_encodings: [],
                rid_to_ssrc: %{}
              ]

  @type id :: String.t()
  @type rid :: String.t()
  @type encoding_key :: :OPUS | :H264 | :VP8
  @type status :: :pending | :ready | :linked | :disabled

  @type t :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t(),
          id: id,
          name: String.t(),
          ssrc: RTP.ssrc_t() | [RTP.ssrc_t()],
          rtx_ssrc: RTP.ssrc_t() | [RTP.ssrc_t()] | nil,
          selected_encoding_key: encoding_key(),
          offered_encodings: [__MODULE__.Encoding.t()],
          status: status(),
          mid: binary(),
          rids: [rid()] | nil,
          rid_to_ssrc: %{},
          extmaps: [Extmap]
        }

  @doc """
  Creates a new track.

  Tracks belonging to the same stream should have the same `stream_id`,
  that can be generated with `stream_id/0`.
  """
  @spec new(:audio | :video, stream_id :: String.t(),
          id: String.t(),
          name: String.t(),
          ssrc: RTP.ssrc_t() | [RTP.ssrc_t()] | nil,
          rtx_ssrc: RTP.ssrc_t() | nil,
          selected_encoding_key: encoding_key(),
          offered_encodings: [__MODULE__.Encoding.t()],
          mid: non_neg_integer(),
          rids: [String.t()] | nil,
          status: status(),
          extmaps: [Extmap]
        ) :: t
  def new(type, stream_id, opts \\ []) do
    id = Keyword.get(opts, :id, Base.encode16(:crypto.strong_rand_bytes(8)))
    name = Keyword.get(opts, :name, "#{id}-#{type}-#{stream_id}")

    %__MODULE__{
      type: type,
      stream_id: stream_id,
      id: id,
      name: name,
      ssrc: Keyword.get(opts, :ssrc, :crypto.strong_rand_bytes(4)),
      rtx_ssrc: Keyword.get(opts, :rtx_ssrc),
      selected_encoding: Keyword.get(opts, :selected_encoding),
      selected_encoding_key: Keyword.get(opts, :selected_encoding_key),
      offered_encodings: Keyword.get(opts, :offered_encodings, []),
      mid: Keyword.get(opts, :mid),
      rids: Keyword.get(opts, :rids),
      status: Keyword.get(opts, :status, :ready),
      extmaps: Keyword.get(opts, :extmaps, [])
    }
  end

  @doc """
  Generates stream id, that can be used to mark tracks belonging to the same stream.
  """
  @spec stream_id() :: String.t()
  def stream_id(), do: UUID.uuid4()

  @doc """
  Determines if the track is using simulcast
  """
  @spec simulcast?(t()) :: boolean()
  def simulcast?(%__MODULE__{rids: rids, ssrc: ssrcs}) when is_list(rids) and is_list(ssrcs),
    do: true

  def simulcast?(%__MODULE__{rids: nil, ssrc: ssrc}) when is_integer(ssrc), do: false

  @doc """
  Creates `t:t/0` from SDP m-line with random track id and stream id.
  """
  @spec from_sdp_media(ExSDP.Media.t()) :: t()
  def from_sdp_media(sdp_media) do
    from_sdp_media(sdp_media, Base.encode16(:crypto.strong_rand_bytes(8)), stream_id())
  end

  @doc """
  Creates `t:t/0` from SDP m-line with specific track id and stream id.
  """
  @spec from_sdp_media(ExSDP.Media.t(), id(), String.t()) :: t()
  def from_sdp_media(sdp_media, id, stream_id) do
    rids = get_rids(sdp_media)

    ssrc_group = ExSDP.get_attribute(sdp_media, :ssrc_group)
    ssrc_attribute = ExSDP.get_attribute(sdp_media, :ssrc)

    [ssrc, rtx_ssrc] =
      cond do
        # case 1 - we have stream with retransmission described by FID ssrc-group
        ssrc_group != nil and ssrc_group.semantics == "FID" -> ssrc_group.ssrcs
        # case 2 - no rtx, just ssrc with cname definition
        ssrc_attribute != nil -> [ssrc_attribute.id, nil]
        # case 3 - simulcast
        # this function is being called only for inbound media
        # therefore, if SSRC is `nil` `sdp_media` must represent simulcast track
        true -> [[], []]
      end

    status =
      if ExSDP.get_attribute(sdp_media, :inactive) != nil do
        :disabled
      else
        :ready
      end

    rtcp_feedbacks =
      sdp_media
      |> ExSDP.get_attributes(:rtcp_feedback)
      |> Enum.group_by(& &1.pt)

    encodings =
      sdp_media
      |> group_params()
      |> Enum.map(fn {rtpmap, fmtps, rtx_fmtp, red_pt} ->
        feedbacks =
          Map.get(rtcp_feedbacks, :all, []) ++ Map.get(rtcp_feedbacks, rtpmap.payload_type, [])

        rtx_metadata =
          case rtx_fmtp do
            nil -> nil
            %FMTP{pt: pt, rtx_time: rtx_time} -> %{payload_type: pt, rtx_time: rtx_time}
          end

        %__MODULE__.Encoding{
          payload_type: rtpmap.payload_type,
          name: rtpmap.encoding,
          clock_rate: rtpmap.clock_rate,
          rtx: rtx_metadata,
          red_payload_type: red_pt,
          audio_channels: rtpmap.params,
          rtcp_feedback: MapSet.new(feedbacks),
          format_params: fmtps
        }
      end)

    opts = [
      id: id,
      ssrc: ssrc,
      rtx_ssrc: rtx_ssrc,
      mid: ExSDP.get_attribute(sdp_media, :mid) |> elem(1),
      rids: rids,
      offered_encodings: encodings,
      status: status,
      extmaps: ExSDP.get_attributes(sdp_media, :extmap)
    ]

    new(sdp_media.type, stream_id, opts)
  end

  @doc """
  Modifies track properties according to provided constraints.

  In particular, after setting constraints, track can be disabled.
  """
  @spec set_constraints(t(), :inbound | :outbound, map()) :: t()
  def set_constraints(track, track_type, constraints) do
    %__MODULE__.Constraints{
      simulcast?: simulcast?,
      codecs_filter: codecs_filter,
      enabled_extensions: enabled_extensions,
      endpoint_direction: endpoint_direction
    } = constraints

    selected_encoding =
      track.offered_encodings
      |> Enum.filter(codecs_filter)
      |> List.first()

    if selected_encoding == nil do
      raise "No supported payload types in SDP offer!"
    end

    status =
      cond do
        track.rids != nil and simulcast? == false ->
          raise RuntimeError, message: "Simulcast was offered, but it's not supported"

        # enforce rid values
        is_list(track.rids) and Enum.any?(track.rids, &(&1 not in @supported_rids)) ->
          Membrane.Logger.debug("""
          Disabling track with id: #{inspect(track.id)} of type: #{inspect(track_type)}.
          Reason: there is unsupported rid in #{inspect(track.rids)}. Supported rids: #{inspect(@supported_rids)}.
          """)

          :disabled

        # check direction
        endpoint_direction == :sendonly and track_type == :inbound ->
          Membrane.Logger.debug("""
          Disabling track with id: #{inspect(track.id)} of type: #{inspect(track_type)}.
          Reason: EndpointBin is set to #{inspect(endpoint_direction)}.
          """)

          :disabled

        true ->
          track.status
      end

    encoding_key = encoding_to_atom(selected_encoding.name)

    extmaps =
      Enum.filter(track.extmaps, &Extension.supported?(enabled_extensions, &1, encoding_key))

    %__MODULE__{
      track
      | selected_encoding_key: encoding_key,
        selected_encoding: selected_encoding,
        offered_encodings: [],
        extmaps: extmaps,
        status: status
    }
  end

  @spec to_otel_data(t()) :: String.t()
  def to_otel_data(%__MODULE__{rids: nil} = track),
    do: "{id: #{track.id}, mid: #{track.mid}, encoding: #{track.selected_encoding_key}}"

  def to_otel_data(%__MODULE__{} = track) do
    rids = Enum.join(track.rids, ", ")

    "{id: #{track.id}, mid: #{track.mid}, encoding: #{track.selected_encoding_key}, rids: [#{rids}]}"
  end

  # TODO: This should be handled by ExSDP
  defp get_rids(sdp_media) do
    sdp_media
    |> ExSDP.get_attributes("rid")
    |> Enum.map(fn {_attr, rid} ->
      # example from sdp
      # a=rid:h send
      # by rid we mean "h send"
      rid |> String.split(" ", parts: 2) |> hd()
    end)
    |> case do
      [] -> nil
      rids -> rids
    end
  end

  defp group_params(sdp_media) do
    # Separate real codecs from RTX and RED streams
    grouped_rtpmaps =
      sdp_media
      |> ExSDP.get_attributes(:rtpmap)
      |> Enum.group_by(fn
        %{encoding: "rtx"} -> :rtx
        %{encoding: "red"} -> :red
        _other -> :codec
      end)

    fmtp_by_pt =
      sdp_media
      |> ExSDP.get_attributes(:fmtp)
      |> Map.new(&{&1.pt, &1})

    rtx_fmtp_by_apt =
      grouped_rtpmaps
      |> Map.get(:rtx, [])
      |> Map.new(fn %RTPMapping{payload_type: pt} ->
        fmtp = fmtp_by_pt |> Map.fetch!(pt)
        {fmtp.apt, fmtp}
      end)

    red_pt_by_apt =
      grouped_rtpmaps
      |> Map.get(:red, [])
      |> Enum.flat_map(fn %RTPMapping{payload_type: pt} ->
        case fmtp_by_pt |> Map.fetch(pt) do
          {:ok, %FMTP{redundant_payloads: apts}} -> apts |> Enum.map(&{&1, pt})
          # Chrome seems to offer an invalid "red" stream for video without FMTP ¯\_(ツ)_/¯
          :error -> []
        end
      end)
      |> Map.new()

    grouped_rtpmaps
    |> Map.get(:codec, [])
    |> Enum.map(fn %RTPMapping{payload_type: pt} = rtpmap ->
      fmtp = fmtp_by_pt[pt]
      rtx_fmtp = rtx_fmtp_by_apt[pt]
      red_payload_type = red_pt_by_apt[pt]

      {rtpmap, fmtp, rtx_fmtp, red_payload_type}
    end)
  end

  defp encoding_to_atom(encoding_name) do
    case encoding_name do
      "opus" -> :OPUS
      "VP8" -> :VP8
      "H264" -> :H264
      encoding -> raise "Not supported encoding: #{encoding}"
    end
  end
end
