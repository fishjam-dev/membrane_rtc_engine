defmodule Membrane.RTC.Engine.Endpoint.Webrtc do
  use Membrane.Bin

  alias Membrane.WebRTC.{SDP, EndpointBin}
  alias Membrane.WebRTC
  alias Membrane.RTC.Engine

  def_options inbound_tracks: [
                spec: [Membrane.WebRTC.Track.t()],
                default: [],
                description: "List of initial inbound tracks"
              ],
              outbound_tracks: [
                spec: [Membrane.WebRTC.Track.t()],
                default: [],
                description: "List of initial outbound tracks"
              ],
              stun_servers: [
                type: :list,
                spec: [ExLibnice.stun_server()],
                default: [],
                description: "List of stun servers"
              ],
              turn_servers: [
                type: :list,
                spec: [ExLibnice.relay_info()],
                default: [],
                description: "List of turn servers"
              ],
              port_range: [
                spec: Range.t(),
                default: 0..0,
                description: "Port range to be used by `Membrane.ICE.Bin`"
              ],
              handshake_opts: [
                type: :list,
                spec: Keyword.t(),
                default: [],
                description: """
                Keyword list with options for handshake module. For more information please
                refer to `Membrane.ICE.Bin`
                """
              ],
              video_codecs: [
                type: :list,
                spec: [ExSDP.Attribute.t()],
                default: [],
                description: "Video codecs that will be passed for SDP offer generation"
              ],
              audio_codecs: [
                type: :list,
                spec: [ExSDP.Attribute.t()],
                default: [],
                description: "Audio codecs that will be passed for SDP offer generation"
              ],
              filter_codecs: [
                spec: ({RTPMapping.t(), FMTP.t() | nil} -> boolean()),
                default: &SDP.filter_mappings(&1),
                description: "Defines function which will filter SDP m-line by codecs"
              ],
              log_metadata: [
                spec: :list,
                spec: Keyword.t(),
                default: [],
                description: "Logger metadata used for endpoint bin and all its descendants"
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request,
    options: [
      encoding: [
        spec: :OPUS | :H264,
        description: "Track encoding"
      ],
      track_enabled: [
        spec: boolean(),
        default: true,
        description: "Enable or disable track"
      ]
    ]

  def_output_pad :output,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request,
    options: [
      track_enabled: [
        spec: boolean(),
        default: true,
        description: "Enable or disable track"
      ],
      packet_filters: [
        spec: [Membrane.RTP.SessionBin.packet_filter_t()],
        default: [],
        description: "List of packet filters that will be applied to the SessionBin's output pad"
      ],
      extensions: [
        spec: [Membrane.RTP.SessionBin.extension_t()],
        default: [],
        description: "List of tuples representing rtp extensions"
      ]
    ]

  @impl true
  def handle_init(opts) do
    endpoint_bin = %EndpointBin{
      stun_servers: opts.stun_servers,
      turn_servers: opts.turn_servers,
      handshake_opts: opts.handshake_opts,
      log_metadata: opts.log_metadata,
      filter_codecs: opts.filter_codecs,
      inbound_tracks: opts.inbound_tracks,
      outbound_tracks: Enum.map(opts.outbound_tracks, &rtc_track_to_webrtc_track(&1))
    }

    spec = %ParentSpec{
      children: %{endpoint_bin: endpoint_bin}
    }

    {{:ok, spec: spec, log_metadata: opts.log_metadata}, %{}}
  end

  @impl true
  def handle_notification({:new_tracks, tracks}, _from, _ctx, state) do
    rtc_tracks = Enum.map(tracks, &webrtc_track_to_rtc_track(&1))
    {{:ok, notify: {:publish, rtc_tracks}}, state}
  end

  @impl true
  def handle_notification({:negotiation_done, new_outbound_tracks}, _from, _ctx, state) do
    {{:ok, notify: {:subscribe, new_outbound_tracks}}, state}
  end

  @impl true
  def handle_notification(notification, _element, _ctx, state) do
    {{:ok, notify: notification}, state}
  end

  @impl true
  def handle_other({:publish, tracks}, _ctx, state) do
    webrtc_tracks =
      Enum.map(
        tracks,
        &WebRTC.Track.new(
          &1.type,
          &1.stream_id,
          struct_to_keyword_list(&1)
        )
      )

    {{:ok, forward: [endpoint_bin: {:add_tracks, webrtc_tracks}]}, state}
  end

  @impl true
  def handle_other(msg, _ctx, state) do
    {{:ok, forward: [endpoint_bin: msg]}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, _track_id) = pad, ctx, state) do
    options = ctx.pads[pad].options |> map_to_keyword_list()

    links = [
      link_bin_input(pad)
      |> via_in(pad, options: options)
      |> to(:endpoint_bin)
    ]

    {{:ok, spec: %ParentSpec{links: links}}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, _track_id) = pad, ctx, state) do
    options =
      ctx.pads[pad].options
      |> map_to_keyword_list()

    spec = %ParentSpec{
      links: [
        link(:endpoint_bin)
        |> via_out(pad,
          options: options
        )
        |> to_bin_output(pad)
      ]
    }

    {{:ok, spec: spec}, state}
  end

  defp webrtc_track_to_rtc_track(track) do
    %Engine.Track{
      type: track.type,
      stream_id: track.stream_id,
      id: track.id,
      encoding: track.encoding,
      fmtp: track.fmtp,
      disabled?: track.status == :disabled
    }
  end

  defp rtc_track_to_webrtc_track(track) do
    track = if track.disabled?, do: Map.put(track, :status, :disabled), else: track
    WebRTC.Track.new(track.type, track.stream_id, struct_to_keyword_list(track))
  end

  defp struct_to_keyword_list(struct),
    do: Map.from_struct(struct) |> map_to_keyword_list()

  defp map_to_keyword_list(map),
    do: Enum.map(map, fn {key, value} -> {key, value} end)
end
