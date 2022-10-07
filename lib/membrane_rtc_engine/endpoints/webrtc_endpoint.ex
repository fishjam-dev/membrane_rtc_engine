defmodule Membrane.RTC.Engine.Endpoint.WebRTC do
  @moduledoc """
  An Endpoint responsible for communicatiing with WebRTC peer.

  It is responsible for sending and receiving media tracks from other WebRTC peer (e.g. web browser).
  """
  use Membrane.Bin

  import Membrane.RTC.Utils

  require Membrane.Logger
  require Membrane.OpenTelemetry
  require Membrane.TelemetryMetrics

  alias ExSDP.Attribute.FMTP
  alias ExSDP.Attribute.RTPMapping
  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.DisplayManager
  alias Membrane.RTC.Engine.Endpoint.WebRTC.{SimulcastConfig, TrackReceiver, TrackSender}
  alias Membrane.RTC.Engine.Event.TrackVadChanged
  alias Membrane.RTC.Engine.Track
  alias Membrane.WebRTC
  alias Membrane.WebRTC.{EndpointBin, SDP}

  @track_metadata_event [Membrane.RTC.Engine, :track, :metadata, :event]
  @peer_metadata_event [Membrane.RTC.Engine, :peer, :metadata, :event]

  @life_span_id "webrtc_endpoint.life_span"

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              direction: [
                spec: EndpointBin.direction(),
                default: :sendrecv,
                description: """
                Direction of WebRTC Endpoint. Determines whether
                EndpointBin can send, receive or both send and receive media.
                For more information refer to t:EndpointBin.direction/0.
                """
              ],
              ice_name: [
                spec: String.t(),
                description: "Ice name is used in creating credentials for ice connnection"
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
              webrtc_extensions: [
                spec: [Membrane.WebRTC.Extension.t()],
                default: [],
                description: """
                List of WebRTC extensions to use.

                At this moment only VAD (RFC 6464) is supported.
                Enabling it will cause RTC Engine sending `{:vad_notification, val, endpoint_id}` messages.
                """
              ],
              extensions: [
                spec: %{
                  (encoding_name :: atom() | :any) => [Membrane.RTP.SessionBin.extension_t()]
                },
                default: %{},
                description: """
                A map pointing from encoding names to lists of extensions that should be used for given encodings.
                Encoding "`:any`" indicates that extensions should be applied regardless of encoding.

                A sample usage would be to add silence discarder to OPUS tracks when VAD extension is enabled.
                It can greatly reduce CPU usage in rooms when there are a lot of people but only a few of
                them are actively speaking.
                """
              ],
              integrated_turn_domain: [
                spec: binary() | nil,
                default: nil,
                description: "Domain address of integrated TURN Servers. Required for TLS TURN"
              ],
              integrated_turn_options: [
                spec: Membrane.ICE.Endpoint.integrated_turn_options_t(),
                default: []
              ],
              owner: [
                spec: pid(),
                description: """
                Pid of parent all notifications will be send to.

                To see possible notifications please refer to module docs.
                """
              ],
              trace_context: [
                spec: :list,
                default: [],
                description: "Trace context for otel propagation"
              ],
              parent_span: [
                spec: :opentelemetry.span_ctx() | nil,
                default: nil,
                description: "Parent span of #{@life_span_id}"
              ],
              video_tracks_limit: [
                spec: integer() | nil,
                default: nil,
                description: """
                Maximal number of video tracks that will be sent to the the browser at the same time.

                This variable indicates how many video tracks should be sent to the browser at the same time.
                If `nil` all video tracks this `#{inspect(__MODULE__)}` receives will be sent.
                """
              ],
              rtcp_receiver_report_interval: [
                spec: Membrane.Time.t() | nil,
                default: nil,
                description:
                  "Receiver reports's generation interval, set to nil to avoid reports generation"
              ],
              rtcp_sender_report_interval: [
                spec: Membrane.Time.t() | nil,
                default: nil,
                description:
                  "Sender reports's generation interval, set to nil to avoid reports generation"
              ],
              simulcast_config: [
                spec: SimulcastConfig.t(),
                default: %SimulcastConfig{},
                description: "Simulcast configuration"
              ],
              peer_metadata: [
                spec: any(),
                default: nil,
                description: "Peer metadata"
              ],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: [],
                description: "Label passed to Membrane.TelemetryMetrics functions"
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
    Membrane.TelemetryMetrics.register(@peer_metadata_event, opts.telemetry_label)

    Membrane.TelemetryMetrics.execute(
      @peer_metadata_event,
      %{metadata: opts.peer_metadata},
      %{},
      opts.telemetry_label
    )

    if opts.trace_context != [], do: Membrane.OpenTelemetry.attach(opts.trace_context)

    start_span_opts =
      case opts.parent_span do
        nil -> []
        parent_span -> [parent_span: parent_span]
      end

    Membrane.OpenTelemetry.start_span(@life_span_id, start_span_opts)

    Membrane.OpenTelemetry.set_attribute(
      @life_span_id,
      :peer_metadata,
      inspect(opts.peer_metadata)
    )

    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{
        outbound_tracks: %{},
        inbound_tracks: %{},
        display_manager: nil,
        endpoint_id_to_action: %{}
      })

    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    log_metadata = state.log_metadata ++ [webrtc_endpoint: endpoint_id]

    endpoint_bin = %EndpointBin{
      handshake_opts: state.handshake_opts,
      log_metadata: log_metadata,
      filter_codecs: state.filter_codecs,
      inbound_tracks: [],
      outbound_tracks: [],
      direction: state.direction,
      extensions: state.webrtc_extensions || [],
      integrated_turn_options: state.integrated_turn_options,
      trace_context: state.trace_context,
      trace_metadata: [ice_name: state.ice_name],
      parent_span: Membrane.OpenTelemetry.get_span(@life_span_id),
      rtcp_receiver_report_interval: state.rtcp_receiver_report_interval,
      rtcp_sender_report_interval: state.rtcp_sender_report_interval,
      simulcast?: state.simulcast_config.enabled,
      telemetry_label: state.telemetry_label
    }

    spec = %ParentSpec{
      children: %{endpoint_bin: endpoint_bin},
      log_metadata: log_metadata
    }

    {:endpoint, endpoint_id} = ctx.name

    {{:ok, spec: spec},
     %{
       state
       | log_metadata: log_metadata,
         display_manager: DisplayManager.new(endpoint_id, state.video_tracks_limit)
     }}
  end

  @impl true
  def handle_notification({:new_tracks, tracks}, :endpoint_bin, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    tracks =
      Enum.map(tracks, fn track ->
        metadata = Map.get(state.track_id_to_metadata, track.id)
        to_rtc_track(track, endpoint_id, metadata)
      end)

    inbound_tracks = update_tracks(tracks, state.inbound_tracks)

    {_actions, state} =
      state.display_manager
      |> DisplayManager.add_inbound_tracks(tracks)
      |> update_display_manager(ctx, state)

    Membrane.OpenTelemetry.add_event(@life_span_id, :publishing_new_tracks,
      tracks_ids: Enum.map(tracks, & &1.id)
    )

    {{:ok, notify: {:publish, {:new_tracks, tracks}}}, %{state | inbound_tracks: inbound_tracks}}
  end

  @impl true
  def handle_notification({:removed_tracks, tracks}, :endpoint_bin, ctx, state) do
    tracks = Enum.map(tracks, &to_rtc_track(&1, Map.get(state.inbound_tracks, &1.id)))
    inbound_tracks = update_tracks(tracks, state.inbound_tracks)

    {actions, state} =
      state.display_manager
      |> DisplayManager.remove_tracks(tracks)
      |> update_display_manager(ctx, state)

    {{:ok, [notify: {:publish, {:removed_tracks, tracks}}] ++ actions},
     %{state | inbound_tracks: inbound_tracks}}
  end

  @impl true
  def handle_notification(
        {:new_track, track_id, rid, encoding, _depayloading_filter},
        _from,
        _ctx,
        state
      ) do
    track_telemetry_label = state.telemetry_label ++ [track_id: "#{track_id}:#{rid}"]

    Membrane.TelemetryMetrics.register(
      @track_metadata_event,
      track_telemetry_label
    )

    Membrane.TelemetryMetrics.execute(
      @track_metadata_event,
      %{metadata: Map.get(state.track_id_to_metadata, track_id)},
      %{},
      track_telemetry_label
    )

    Membrane.OpenTelemetry.add_event(@life_span_id, :track_ready, track_id: track_id)

    variant = to_track_variant(rid)

    {{:ok, notify: {:track_ready, track_id, variant, encoding}}, state}
  end

  @impl true
  def handle_notification({:negotiation_done, new_outbound_tracks}, _from, ctx, state) do
    new_outbound_tracks =
      Enum.map(new_outbound_tracks, &to_rtc_track(&1, Map.get(state.outbound_tracks, &1.id)))

    {:endpoint, endpoint_id} = ctx.name

    Enum.each(new_outbound_tracks, fn track ->
      case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
        :ok ->
          Membrane.OpenTelemetry.add_event(@life_span_id, :subscribing_on_track,
            track_id: track.id
          )

          :ok

        {:error, :invalid_track_id} ->
          Membrane.Logger.debug("""
          Couldn't subscribe to track: #{inspect(track.id)}. No such track.
          It was probably removed before we restarted ICE. Ignoring.
          """)

        {:error, reason} ->
          Membrane.OpenTelemetry.add_event(@life_span_id, :subscribing_on_track_error,
            track_id: track.id,
            reason: inspect(reason)
          )

          raise "Couldn't subscribe to track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
      end
    end)

    {actions, state} =
      state.display_manager
      |> DisplayManager.subscribe_tracks(new_outbound_tracks)
      |> update_display_manager(ctx, state)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_notification({:vad, val}, :endpoint_bin, _ctx, state) do
    audio_inbound_track = state.inbound_tracks |> Map.values() |> Enum.find(&(&1.type == :audio))

    vad_notification = %TrackVadChanged{
      vad_value: val,
      timestamp: DisplayManager.get_monotonic_time()
    }

    {{:ok, forward: {{:track_sender, audio_inbound_track.id}, vad_notification}}, state}
  end

  @impl true
  def handle_notification(
        {:signal, {:offer_data, media_count, turns}},
        _element,
        _ctx,
        state
      ) do
    turns = get_turn_configs(turns, state)
    media_event = serialize({:signal, {:offer_data, media_count, turns}})

    Membrane.OpenTelemetry.add_event(@life_span_id, :custom_media_event_sent,
      event: inspect(media_event)
    )

    {{:ok, notify: {:custom_media_event, media_event}}, state}
  end

  @impl true
  def handle_notification(
        {:signal, _notification} = media_event_data,
        _element,
        _ctx,
        state
      ) do
    media_event = serialize(media_event_data)

    Membrane.OpenTelemetry.add_event(@life_span_id, :custom_media_event_sent,
      event: inspect(media_event)
    )

    {{:ok, notify: {:custom_media_event, media_event}}, state}
  end

  @impl true
  def handle_notification(
        {:variant_switched, new_variant},
        {:track_receiver, track_id},
        _ctx,
        state
      ) do
    track = Map.fetch!(state.outbound_tracks, track_id)

    media_event = %{
      type: "encodingSwitched",
      data: %{
        peerId: track.origin,
        trackId: track_id,
        encoding: to_rid(new_variant)
      }
    }

    {{:ok, notify: {:custom_media_event, media_event}}, state}
  end

  @impl true
  def handle_notification(
        %TrackVadChanged{vad_value: vad_value, timestamp: timestamp},
        {:track_receiver, track_id},
        ctx,
        state
      ) do
    endpoint_id = Map.fetch!(state.outbound_tracks, track_id).origin

    {actions, state} =
      state.display_manager
      |> DisplayManager.add_vad_notification(endpoint_id, vad_value, timestamp)
      |> update_display_manager(ctx, state)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_notification(notification, _element, _ctx, state),
    do: {{:ok, notify: notification}, state}

  @impl true
  def handle_other({:new_tracks, tracks}, ctx, state) do
    # Don't subscribe for new tracks yet.
    # We will do this after ice restart is finished.
    # Notification :negotiation_done  will inform us about it

    webrtc_tracks =
      Enum.map(
        tracks,
        &to_webrtc_track(&1)
      )

    outbound_tracks = update_tracks(tracks, state.outbound_tracks)

    {{:ok, forward(:endpoint_bin, {:add_tracks, webrtc_tracks}, ctx)},
     %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_other({:custom_media_event, event}, ctx, state) do
    case deserialize(event) do
      {:ok, data} ->
        Membrane.OpenTelemetry.add_event(@life_span_id, :custom_media_event_received,
          type: data[:type],
          data: inspect(data[:data])
        )

        handle_custom_media_event(data, ctx, state)

      {:error, :invalid_media_event} ->
        Membrane.OpenTelemetry.add_event(@life_span_id, :invalid_custom_media_event_received,
          event: inspect(event)
        )

        Membrane.Logger.warn("Invalid media event #{inspect(event)}. Ignoring.")
        {:ok, state}
    end
  end

  @impl true
  def handle_other(msg, ctx, state) do
    {{:ok, forward(:endpoint_bin, msg, ctx)}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    track = Map.fetch!(state.outbound_tracks, track_id)
    initial_target_variant = state.simulcast_config.initial_target_variant.(track)

    links = [
      link_bin_input(pad)
      |> to({:track_receiver, track_id}, %TrackReceiver{
        track: track,
        initial_target_variant: initial_target_variant
      })
      |> via_in(pad, options: [use_payloader?: false])
      |> to(:endpoint_bin)
    ]

    {action, endpoint_id_to_action} =
      Map.pop(state.endpoint_id_to_action, {:track_receiver, track_id})

    state = %{state | endpoint_id_to_action: endpoint_id_to_action}

    actions = if action == nil, do: [], else: [action]

    {{:ok, [spec: %ParentSpec{log_metadata: state.log_metadata, links: links}] ++ actions}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {track_id, variant}) = pad, ctx, state) do
    %Track{encoding: encoding} = track = Map.get(state.inbound_tracks, track_id)
    extensions = Map.get(state.extensions, encoding, []) ++ Map.get(state.extensions, :any, [])
    track_sender = {:track_sender, track_id}

    link_to_track_sender =
      if Map.has_key?(ctx.children, track_sender) do
        &to(&1, track_sender)
      else
        &to(&1, track_sender, %TrackSender{track: track})
      end

    # EndpointBin expects `rid` to be nil for non simulcast tracks
    # assume that tracks with [:high] variants are non-simulcast
    rid = if state.inbound_tracks[track_id].variants == [:high], do: nil, else: to_rid(variant)

    spec = %ParentSpec{
      links: [
        link(:endpoint_bin)
        |> via_out(Pad.ref(:output, {track_id, rid}),
          options: [extensions: extensions, use_depayloader?: false]
        )
        |> via_in(Pad.ref(:input, {track_id, variant}))
        |> then(link_to_track_sender)
        |> via_out(pad)
        |> to_bin_output(pad)
      ],
      log_metadata: state.log_metadata
    }

    {{:ok, spec: spec}, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, track_id), _ctx, state) do
    {{:ok, remove_child: {:track_receiver, track_id}}, state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {track_id, _variant}), ctx, state) do
    # check if there are any variants that are still linked
    track_linked? =
      ctx.pads
      |> Map.keys()
      |> Enum.any?(&match?(Pad.ref(:output, {^track_id, _variant}), &1))

    if track_linked? do
      {:ok, state}
    else
      {{:ok, remove_child: {:track_sender, track_id}}, state}
    end
  end

  defp handle_custom_media_event(%{type: :sdp_offer, data: data}, ctx, state) do
    state = Map.put(state, :track_id_to_metadata, data.track_id_to_track_metadata)
    msg = {:signal, {:sdp_offer, data.sdp_offer.sdp, data.mid_to_track_id}}
    {{:ok, forward(:endpoint_bin, msg, ctx)}, state}
  end

  defp handle_custom_media_event(%{type: :candidate, data: data}, ctx, state) do
    msg = {:signal, {:candidate, data.candidate}}
    {{:ok, forward(:endpoint_bin, msg, ctx)}, state}
  end

  defp handle_custom_media_event(%{type: :renegotiate_tracks}, ctx, state) do
    msg = {:signal, :renegotiate_tracks}
    {{:ok, forward(:endpoint_bin, msg, ctx)}, state}
  end

  defp handle_custom_media_event(%{type: :set_target_track_variant, data: data}, ctx, state) do
    msg = {:set_target_variant, to_track_variant(data.variant)}
    {{:ok, forward({:track_receiver, data.track_id}, msg, ctx)}, state}
  end

  defp handle_custom_media_event(%{type: :prioritize_track, data: data}, ctx, state) do
    # msg = {:prioritize_track, ctx.name, data.track_id}
    # send_if_not_nil(state.display_manager, msg)
    {actions, state} =
      state.display_manager
      |> DisplayManager.prioritize_track(data.track_id)
      |> update_display_manager(ctx, state)

    {{:ok, actions}, state}
  end

  defp handle_custom_media_event(%{type: :unprioritize_track, data: data}, ctx, state) do
    # msg = {:unprioritize_track, ctx.name, data.track_id}
    # send_if_not_nil(state.display_manager, msg)
    {actions, state} =
      state.display_manager
      |> DisplayManager.unprioritize_track(data.track_id)
      |> update_display_manager(ctx, state)

    {{:ok, actions}, state}
  end

  defp handle_custom_media_event(%{type: :prefered_video_sizes, data: data}, ctx, state) do
    {_actions, state} =
      state.display_manager
      |> DisplayManager.change_prefered_video_sizes(
        data.big_screens,
        data.medium_screens,
        data.small_screens,
        data.same_size?
      )
      |> update_display_manager(ctx, state)

    {:ok, state}
  end

  defp get_turn_configs(turn_servers, state) do
    Enum.map(turn_servers, fn
      %{secret: secret} = turn_server ->
        {username, password} = generate_turn_credentials(state.ice_name, secret)

        case state.integrated_turn_domain do
          nil -> turn_server
          domain -> Map.put(turn_server, :domain_name, domain)
        end
        |> Map.delete(:secret)
        |> Map.put(:username, username)
        |> Map.put(:password, password)

      other ->
        other
    end)
  end

  defp update_tracks(tracks, track_id_to_track),
    do:
      Enum.reduce(tracks, track_id_to_track, fn track, acc ->
        Map.put(acc, track.id, track)
      end)

  defp serialize({:signal, {:sdp_answer, answer, mid_to_track_id}}),
    do: %{
      type: "sdpAnswer",
      data: %{
        type: "answer",
        sdp: answer,
        midToTrackId: mid_to_track_id
      }
    }

  defp serialize({:signal, {:offer_data, tracks_types, turns}}) do
    integrated_turn_servers =
      Enum.map(turns, fn turn ->
        addr =
          if turn.relay_type == :tls and turn[:domain_name],
            do: turn[:domain_name],
            else: :inet.ntoa(turn.mocked_server_addr) |> to_string()

        %{
          serverAddr: addr,
          serverPort: turn.server_port,
          transport: turn.relay_type,
          password: turn.password,
          username: turn.username
        }
      end)

    %{
      type: "offerData",
      data: %{
        tracksTypes: tracks_types,
        integratedTurnServers: integrated_turn_servers
      }
    }
  end

  defp serialize({:signal, {:candidate, candidate, sdp_m_line_index}}),
    do: %{
      type: "candidate",
      data: %{
        candidate: candidate,
        sdpMLineIndex: sdp_m_line_index,
        sdpMid: nil,
        usernameFragment: nil
      }
    }

  defp serialize({:signal, {:sdp_offer, offer}}),
    do: %{
      type: "sdpOffer",
      data: %{
        type: "offer",
        sdp: offer
      }
    }

  defp deserialize(%{"type" => "renegotiateTracks"}) do
    {:ok, %{type: :renegotiate_tracks}}
  end

  defp deserialize(%{"type" => "prioritizeTrack"} = event) do
    case event do
      %{"type" => "prioritizeTrack", "data" => %{"trackId" => track_id}} ->
        {:ok, %{type: :prioritize_track, data: %{track_id: track_id}}}
    end
  end

  defp deserialize(%{"type" => "unprioritizeTrack"} = event) do
    case event do
      %{"type" => "unprioritizeTrack", "data" => %{"trackId" => track_id}} ->
        {:ok, %{type: :unprioritize_track, data: %{track_id: track_id}}}
    end
  end

  defp deserialize(%{"type" => "preferedVideoSizes"} = event) do
    case event do
      %{
        "type" => "preferedVideoSizes",
        "data" => %{
          "bigScreens" => big_screens,
          "mediumScreens" => medium_screens,
          "smallScreens" => small_screens,
          "allSameSize" => same_size?
        }
      } ->
        {:ok,
         %{
           type: :prefered_video_sizes,
           data: %{
             big_screens: big_screens,
             medium_screens: medium_screens,
             small_screens: small_screens,
             same_size?: same_size?
           }
         }}
    end
  end

  defp deserialize(%{"type" => "candidate"} = event) do
    case event do
      %{
        "type" => "candidate",
        "data" => %{
          "candidate" => candidate,
          "sdpMLineIndex" => sdp_m_line_index
        }
      } ->
        {:ok,
         %{
           type: :candidate,
           data: %{
             candidate: candidate,
             sdp_m_line_index: sdp_m_line_index
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp deserialize(%{"type" => "sdpOffer"} = event) do
    case event do
      %{
        "type" => "sdpOffer",
        "data" => %{
          "sdpOffer" => %{
            "type" => "offer",
            "sdp" => sdp
          },
          "trackIdToTrackMetadata" => track_id_to_track_metadata,
          "midToTrackId" => mid_to_track_id
        }
      } ->
        {:ok,
         %{
           type: :sdp_offer,
           data: %{
             sdp_offer: %{
               type: :offer,
               sdp: sdp
             },
             track_id_to_track_metadata: track_id_to_track_metadata,
             mid_to_track_id: mid_to_track_id
           }
         }}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp deserialize(%{"type" => "setTargetTrackVariant"} = event) do
    case event do
      %{
        "type" => "setTargetTrackVariant",
        "data" => %{
          "trackId" => tid,
          "variant" => variant
        }
      } ->
        {:ok, %{type: :set_target_track_variant, data: %{track_id: tid, variant: variant}}}

      _other ->
        {:error, :invalid_media_event}
    end
  end

  defp to_rtc_track(%WebRTC.Track{} = track, %Engine.Track{} = original_track) do
    to_rtc_track(track, original_track.origin, original_track.metadata)
  end

  defp to_rtc_track(%WebRTC.Track{} = track, origin, metadata) do
    extension_key = WebRTC.Extension

    variants = Enum.map(track.rids || [nil], &to_track_variant(&1))

    Engine.Track.new(
      track.type,
      track.stream_id,
      origin,
      track.encoding,
      track.rtp_mapping.clock_rate,
      track.fmtp,
      id: track.id,
      variants: variants,
      active?: track.status != :disabled,
      metadata: metadata,
      ctx: %{extension_key => track.extmaps}
    )
  end

  defp to_webrtc_track(%Engine.Track{} = track) do
    track = if track.active?, do: track, else: Map.put(track, :status, :disabled)
    extmaps = Map.get(track.ctx, WebRTC.Extension, [])
    track = Map.put(track, :extmaps, extmaps)
    WebRTC.Track.new(track.type, track.stream_id, to_keyword_list(track))
  end

  defp to_keyword_list(%Engine.Track{} = struct),
    do: Map.from_struct(struct) |> to_keyword_list()

  defp to_keyword_list(%{} = map), do: Enum.map(map, fn {key, value} -> {key, value} end)

  defp to_track_variant(rid) when rid in ["h", nil], do: :high
  defp to_track_variant("m"), do: :medium
  defp to_track_variant("l"), do: :low

  defp to_rid(:high), do: "h"
  defp to_rid(:medium), do: "m"
  defp to_rid(:low), do: "l"

  defp update_display_manager({running_track_ids, stopped_track_ids, display_manager}, ctx, state) do
    state = %{state | display_manager: display_manager}
    actions = DisplayManager.map_tracks_to_actions(running_track_ids, stopped_track_ids)

    {actions, state} =
      Enum.reduce(actions, {[], state}, fn {:forward, {element_name, _msg_type}} = action,
                                           {actions, state} ->
        if find_child(ctx, pattern: ^element_name) == nil do
          state = put_in(state, [:endpoint_id_to_action, element_name], action)
          {actions, state}
        else
          {[action | actions], state}
        end
      end)

    {actions, state}
  end

  defp update_display_manager(display_manager, _ctx, state) do
    state = %{state | display_manager: display_manager}
    {[], state}
  end
end
