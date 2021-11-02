defmodule Membrane.RTC.Engine.Endpoint.Webrtc do
  use Membrane.Bin

  alias Membrane.WebRTC.EndpointBin
  alias Membrane.WebRTC.SDP

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
      outbound_tracks: opts.outbound_tracks
    }

    spec = %ParentSpec{
      children: %{endpoint_bin: endpoint_bin}
    }

    {{:ok, spec: spec, log_metadata: opts.log_metadata}, %{}}
  end

  @impl true
  def handle_notification(notification, _element, _ctx, state) do
    {{:ok, notify: notification}, state}
  end

  @impl true
  def handle_other(msg, _ctx, state) do
    {{:ok, forward: [endpoint_bin: msg]}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, _track_id) = pad, ctx, state) do
    options = ctx.pads[pad].options |> Enum.map(fn {key, value} -> {key, value} end)

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
      |> Enum.map(fn {key, value} -> {key, value} end)

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
end
