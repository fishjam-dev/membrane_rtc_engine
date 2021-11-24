defmodule Membrane.RTC.Engine.Endpoint.WebRTC do
  @moduledoc """
  An Endpoint responsible for communicatiing with WebRTC peer.

  It is responsible for sending and receiving media tracks from other WebRTC peer (e.g. web browser).
  """
  use Membrane.Bin

  alias Membrane.WebRTC.{SDP, EndpointBin}
  alias Membrane.WebRTC
  alias Membrane.RTC.Engine

  @type stun_server_t() :: ExLibnice.stun_server()
  @type turn_server_t() :: ExLibnice.relay_info()

  @type extension_options_t() :: [
          vad: boolean()
        ]

  @type packet_filters_t() :: %{
          (encoding_name :: atom()) => [Membrane.RTP.SessionBin.packet_filter_t()]
        }

  def_options stun_servers: [
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
              ],
              extension_options: [
                spec: extension_options_t(),
                default: [vad: false],
                description: """
                List of RTP extensions to use.

                At this moment only `vad` extension is supported.
                Enabling it will cause SFU sending `{:vad_notification, val, endpoint_id}` messages.
                """
              ],
              packet_filters: [
                spec: packet_filters_t(),
                default: %{},
                description: """
                A map pointing from encoding names to lists of packet filters that should be used for given encodings.

                A sample usage would be to add silence discarder to OPUS tracks when VAD extension is enabled.
                It can greatly reduce CPU usage in rooms when there are a lot of people but only a few of
                them are actively speaking.
                """
              ],
              owner: [
                spec: pid(),
                description: """
                Pid of parent to which will be send notification from bin e.g

                `{:vad_notification, val, endpoint}`
                """
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  def_output_pad :output,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  @impl true
  def handle_init(opts) do
    endpoint_bin = %EndpointBin{
      stun_servers: opts.stun_servers,
      turn_servers: opts.turn_servers,
      handshake_opts: opts.handshake_opts,
      log_metadata: opts.log_metadata,
      filter_codecs: opts.filter_codecs,
      inbound_tracks: [],
      outbound_tracks: []
    }

    spec = %ParentSpec{
      children: %{endpoint_bin: endpoint_bin}
    }

    state = %{
      extensions: opts.extension_options,
      packet_filters: opts.packet_filters || %{},
      outbound_tracks: %{},
      inbound_tracks: %{},
      owner: opts.owner
    }

    {{:ok, spec: spec, log_metadata: opts.log_metadata}, state}
  end

  @impl true
  def handle_notification({:new_tracks, tracks}, _from, _ctx, state) do
    tracks = Enum.map(tracks, &to_rtc_track(&1))
    inbound_tracks = update_tracks(tracks, state.inbound_tracks)
    publish_notification = Engine.publish({:new_tracks, tracks})
    {{:ok, publish_notification}, %{state | inbound_tracks: inbound_tracks}}
  end

  @impl true
  def handle_notification({:removed_tracks, tracks}, _from, _ctx, state) do
    {tracks, outbound_tracks} = update_tracks(tracks, state.inbound_tracks)

    {{:ok, notify: {:publish, {:removed_tracks, tracks}}},
     %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_notification(
        {:new_track, track_id, encoding, depayloading_filter},
        _from,
        _ctx,
        state
      ) do
    {{:ok, Engine.track_ready(track_id, encoding, depayloading_filter)}, state}
  end

  @impl true
  def handle_notification({:negotiation_done, new_outbound_tracks}, _from, _ctx, state) do
    tracks = Enum.map(new_outbound_tracks, fn track -> {track.id, :RTP} end)
    subscriptions = Engine.subscribe(tracks)
    {{:ok, subscriptions}, state}
  end

  @impl true
  def handle_notification({:vad, val}, {:endpoint, endpoint_id}, _ctx, state) do
    send(state.owner, {:vad_notification, val, endpoint_id})
    {:ok, state}
  end

  @impl true
  def handle_notification(notification, _element, _ctx, state),
    do: {{:ok, notify: notification}, state}

  @impl true
  def handle_other({:new_tracks, tracks}, _ctx, state) do
    # Don't subscribe for new tracks yet.
    # We will do this after ice restart is finished.
    # Notification :negotiation_done  will inform us about it

    webrtc_tracks =
      Enum.map(
        tracks,
        &to_webrtc_track(&1)
      )

    outbound_tracks = update_tracks(tracks, state.outbound_tracks)

    {{:ok, forward: [endpoint_bin: {:add_tracks, webrtc_tracks}]},
     %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_other(msg, _ctx, state) do
    {{:ok, forward: [endpoint_bin: msg]}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, _track_id) = pad, _ctx, state) do
    links = [
      link_bin_input(pad)
      |> via_in(pad, options: [use_payloader?: false])
      |> to(:endpoint_bin)
    ]

    {{:ok, spec: %ParentSpec{links: links}}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, track_id) = pad, _ctx, state) do
    %{encoding: encoding} = Map.get(state.inbound_tracks, track_id)
    extensions = setup_extensions(encoding, state[:options][:extension_options])
    packet_filters = state.packet_filters[encoding] || []

    spec = %ParentSpec{
      links: [
        link(:endpoint_bin)
        |> via_out(pad,
          options: [
            packet_filters: packet_filters,
            extensions: extensions,
            use_depayloader?: false
          ]
        )
        |> to_bin_output(pad)
      ]
    }

    {{:ok, spec: spec}, state}
  end

  defp update_tracks(tracks, track_id_to_track),
    do:
      Enum.reduce(tracks, track_id_to_track, fn track, acc ->
        Map.put(acc, track.id, track)
      end)

  defp setup_extensions(encoding, extension_options) do
    if encoding == :OPUS and extension_options[:vad], do: [{:vad, Membrane.RTP.VAD}], else: []
  end

  defp to_rtc_track(%WebRTC.Track{} = track) do
    %Engine.Track{
      type: track.type,
      stream_id: track.stream_id,
      id: track.id,
      encoding: track.encoding,
      format: [:RTP, :raw],
      fmtp: track.fmtp,
      active?: track.status != :disabled
    }
  end

  defp to_webrtc_track(%Engine.Track{} = track) do
    track = if track.active?, do: track, else: Map.put(track, :status, :disabled)
    WebRTC.Track.new(track.type, track.stream_id, to_keyword_list(track))
  end

  defp to_keyword_list(%_{} = struct), do: Map.from_struct(struct) |> to_keyword_list()

  defp to_keyword_list(%{} = map), do: Enum.map(map, fn {key, value} -> {key, value} end)
end
