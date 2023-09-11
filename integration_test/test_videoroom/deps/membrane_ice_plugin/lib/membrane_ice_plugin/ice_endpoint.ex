defmodule Membrane.ICE.Endpoint do
  @moduledoc """
  Endpoint used for establishing ICE connection, sending and receiving messages.

  ### Architecture and pad semantic
  Both input and output pads are dynamic ones.
  One instance of ICE Endpoint is responsible for handling only one ICE stream with only one component.

  ### Linking using output pad
  To receive messages after establishing ICE connection you have to link ICE Endpoint to your element
  via `Pad.ref(:output, 1)`. `1` is an id of component from which your element will receive messages - because
  there will be always at most one component, id of it will be equal `1`.

  **Important**: you can link to ICE Endpoint using its output pad in any moment you want but if you don't
  want to miss any messages do it before playing your pipeline.

  ### Linking using input pad
  To send messages after establishing ICE connection you have to link to ICE Endpoint via
  `Pad.ref(:input, 1)`. `1` is an id of component which will be used to send
  messages via net. To send data from multiple elements via the same component you have to
  use [membrane_funnel_plugin](https://github.com/membraneframework/membrane_funnel_plugin).

  ### Notifications API

  ICE Endpoint handles the following notifications:

  - `:gather_candidates`

  - `{:set_remote_credentials, credentials}` - credentials are string in form of "ufrag passwd"

  - `:peer_candidate_gathering_done`

  ICE Endpoint sends the following notifications:

  - `{:new_candidate_full, candidate}`
    Triggered by: `:gather_candidates`

  - `{:udp_integrated_turn, udp_integrated_turn}`

  - `{:handshake_init_data, component_id, handshake_init_data}`

  - `{:connection_ready, stream_id, component_id}`

  - `{:component_state_failed, stream_id, component_id}`

  ### Sending and receiving messages
  To send or receive messages just link to ICE Endpoint using relevant pads.
  As soon as connection is established your element will receive demands and incoming messages.

  ### Establishing a connection

  #### Gathering ICE candidates
  Data about integrated TURN servers set up by `Membrane.ICE.Endpoint`, passed to the parent via notification, should be
  forwarded to the second peer, that will try to establish ICE connection with `Membrane.ICE.Endpoint`. The second peer
  should have at least one allocation, in any of running integrated TURN servers (Firefox or Chrome will probably
  have one allocation per TURN Server)

  #### Performing ICE connectivity checks, selecting candidates pair
  All ICE candidates from the second peer, that are not relay candidates corresponded to allocations on integrated TURN
  servers, will be ignored. Every ICE connectivity check sent via integrated TURN server is captured, parsed, and
  forwarded to ICE Endpoint in message `{:connectivity_check, attributes, allocation_pid}`. ICE Endpoint sends to
  messages in form of `{:send_connectivity_check, attributes}` on `allocation_pid`, to send his connectivity checks
  to the second peer. Role of ICE Endpoint can be ice-controlled, but cannot be ice-controlling. It is suggested, to use
  `ice-lite` option in SDP message, but it is not necessary. ICE Endpoint supports both, aggressive and normal nomination.
  After starting ICE or after every ICE restart, ICE Endpoint will pass all traffic and connectivity checks via
  allocation, which corresponds to the last selected ICE candidates pair.
  """

  use Membrane.Endpoint

  require Membrane.Logger
  require Membrane.OpenTelemetry
  require Membrane.TelemetryMetrics

  alias __MODULE__.Allocation
  alias Membrane.Funnel
  alias Membrane.ICE.{CandidatePortAssigner, Utils}
  alias Membrane.RemoteStream
  alias Membrane.SRTP
  alias Membrane.TelemetryMetrics

  @component_id 1
  @stream_id 1
  @time_between_keepalives 1_000_000_000
  @ice_restart_timeout 5_000

  @payload_received_event [Membrane.ICE, :ice, :payload, :received]
  @payload_sent_event [Membrane.ICE, :ice, :payload, :sent]
  @request_received_event [Membrane.ICE, :stun, :request, :received]
  @response_sent_event [Membrane.ICE, :stun, :response, :sent]
  @indication_sent_event [Membrane.ICE, :stun, :indication, :sent]
  @ice_port_assigned [Membrane.ICE, :port, :assigned]
  @send_error_event [Membrane.ICE, :ice, :send_errors]
  @buffer_processing_time [Membrane.ICE, :ice, :buffer, :processing_time]

  @emitted_events [
    @payload_received_event,
    @payload_sent_event,
    @request_received_event,
    @response_sent_event,
    @indication_sent_event,
    @ice_port_assigned,
    @send_error_event,
    @buffer_processing_time
  ]

  @life_span_id "ice_endpoint.life_span"
  @dtls_handshake_span_id "ice_endpoint.dtls_handshake"
  @alloc_span_name "ice_endpoint.turn_allocation"

  @typedoc """
  Options defining the behavior of ICE.Endpoint in relation to integrated TURN servers.
  - `:ip` - IP, where integrated TURN server will open its sockets
  - `:mock_ip` - IP, that will be part of the allocation address contained in Allocation Succes
  message. Because of the fact, that in integrated TURNS no data is relayed via allocation address,
  there is no need to open socket there. There are some cases, where it is necessary, to tell
  the browser, that we have opened allocation on different IP, that we have TURN listening on,
  eg. we are using Docker container
  - `:ports_range` - range, where integrated TURN server will try to open ports
  - `:cert_file` - path to file with certificate and private key, used for estabilishing TLS connection
  for TURN using TLS over TCP
  """
  @type integrated_turn_options_t() :: [
          ip: :inet.ip4_address() | nil,
          mock_ip: :inet.ip4_address() | nil,
          ports_range: {:inet.port_number(), :inet.port_number()} | nil,
          cert_file: binary() | nil
        ]

  def_options dtls?: [
                spec: boolean(),
                default: true,
                description: "`true`, if using DTLS Handshake, `false` otherwise"
              ],
              ice_lite?: [
                spec: boolean(),
                default: true,
                description:
                  "`true`, when ice-lite option was send in SDP message, `false` otherwise"
              ],
              handshake_opts: [
                spec: keyword(),
                default: [],
                description:
                  "Options for `ExDTLS` module. They will be passed to `ExDTLS.start_link/1`"
              ],
              integrated_turn_options: [
                spec: [integrated_turn_options_t()],
                description: "Integrated TURN Options"
              ],
              telemetry_label: [
                spec: TelemetryMetrics.label(),
                default: [],
                description: "Label passed to Membrane.TelemetryMetrics functions"
              ],
              trace_context: [
                spec: :list | any(),
                default: [],
                description: "Trace context for otel propagation"
              ],
              parent_span: [
                spec: :opentelemetry.span_ctx() | nil,
                default: nil,
                description: "Parent span of #{@life_span_id}"
              ]

  def_input_pad :input,
    availability: :on_request,
    accepted_format: _any,
    mode: :pull,
    demand_unit: :buffers

  def_output_pad :output,
    availability: :on_request,
    accepted_format: %RemoteStream{content_format: nil, type: :packetized},
    mode: :push

  defmodule Allocation do
    @enforce_keys [:pid]

    # field `:in_nominated_pair` says, whenether or not, specific allocation
    # is a browser ICE candidate, that belongs to nominated ICE candidates pair
    defstruct @enforce_keys ++ [magic: nil, in_nominated_pair: false]
  end

  @impl true
  def handle_init(_context, options) do
    %__MODULE__{
      integrated_turn_options: integrated_turn_options,
      dtls?: dtls?,
      handshake_opts: hsk_opts,
      telemetry_label: telemetry_label,
      trace_context: trace_context,
      parent_span: parent_span
    } = options

    if trace_context != [], do: Membrane.OpenTelemetry.attach(trace_context)
    start_span_opts = if parent_span, do: [parent_span: parent_span], else: []
    Membrane.OpenTelemetry.start_span(@life_span_id, start_span_opts)

    for event_name <- @emitted_events do
      TelemetryMetrics.register(event_name, telemetry_label)
    end

    state = %{
      id: to_string(Enum.map(1..10, fn _i -> Enum.random(?a..?z) end)),
      turn_allocs: %{},
      integrated_turn_options: integrated_turn_options,
      fake_candidate_ip: integrated_turn_options[:mock_ip] || integrated_turn_options[:ip],
      selected_alloc: nil,
      dtls?: dtls?,
      hsk_opts: hsk_opts,
      telemetry_label: telemetry_label,
      component_connected?: false,
      cached_hsk_packets: nil,
      component_ready?: false,
      pending_connection_ready?: false,
      connection_status_sent?: false,
      sdp_offer_arrived?: false,
      ice_restart_timer: nil,
      first_dtls_hsk_packet_arrived: false
    }

    {[], state}
  end

  @impl true
  def handle_playing(ctx, %{dtls?: true} = state) do
    case CandidatePortAssigner.assign_candidate_port() do
      {:ok, candidate_port} ->
        {:ok, dtls} = ExDTLS.start_link(state.hsk_opts)
        {:ok, fingerprint} = ExDTLS.get_cert_fingerprint(dtls)
        hsk_state = %{:dtls => dtls, :client_mode => state.hsk_opts[:client_mode]}
        ice_ufrag = Utils.generate_ice_ufrag()
        ice_pwd = Utils.generate_ice_pwd()

        [udp_integrated_turn] =
          Utils.start_integrated_turn_servers([:udp], state.integrated_turn_options,
            parent: self()
          )

        Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
          Membrane.ICE.Utils.stop_integrated_turn(udp_integrated_turn)
        end)

        state =
          Map.merge(state, %{
            candidate_port: candidate_port,
            udp_integrated_turn: udp_integrated_turn,
            local_ice_pwd: ice_pwd,
            handshake: %{state: hsk_state, status: :in_progress, keying_material_event: nil}
          })
          |> start_ice_restart_timer()

        Membrane.TelemetryMetrics.execute(
          @ice_port_assigned,
          %{port: udp_integrated_turn.server_port, protocol: udp_integrated_turn.relay_type},
          %{},
          state.telemetry_label
        )

        actions = [
          stream_format: {Pad.ref(:output, @component_id), %RemoteStream{type: :packetized}},
          start_timer: {:keepalive_timer, @time_between_keepalives},
          notify_parent: {:udp_integrated_turn, udp_integrated_turn},
          notify_parent: {:handshake_init_data, @component_id, fingerprint},
          notify_parent: {:local_credentials, "#{ice_ufrag} #{ice_pwd}"}
        ]

        {actions, state}

      {:error, :no_free_candidate_port} = err ->
        raise "ICE: No free candidate port available. #{inspect(err)}"
    end
  end

  @impl true
  def handle_playing(ctx, state) do
    case CandidatePortAssigner.assign_candidate_port() do
      {:ok, candidate_port} ->
        ice_ufrag = Utils.generate_ice_ufrag()
        ice_pwd = Utils.generate_ice_pwd()

        [udp_integrated_turn] =
          Utils.start_integrated_turn_servers([:udp], state.integrated_turn_options,
            parent: self()
          )

        Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
          Membrane.ICE.Utils.stop_integrated_turn(udp_integrated_turn)
        end)

        state =
          Map.merge(state, %{
            candidate_port: candidate_port,
            udp_integrated_turn: udp_integrated_turn,
            local_ice_pwd: ice_pwd
          })
          |> start_ice_restart_timer()

        Membrane.TelemetryMetrics.execute(
          @ice_port_assigned,
          %{port: udp_integrated_turn.server_port, protocol: udp_integrated_turn.relay_type},
          %{},
          state.telemetry_label
        )

        actions = [
          notify_parent: {:udp_integrated_turn, udp_integrated_turn},
          notify_parent: {:handshake_init_data, @component_id, nil},
          notify_parent: {:local_credentials, "#{ice_ufrag} #{ice_pwd}"}
        ]

        {actions, state}

      {:error, :no_free_candidate_port} = err ->
        raise "ICE: No free candidate port available. #{inspect(err)}"
    end
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, @component_id), ctx, state) do
    actions = maybe_send_demands_actions(ctx, state)
    {actions, state}
  end

  @impl true
  def handle_pad_added(
        Pad.ref(:output, @component_id) = pad,
        ctx,
        %{dtls?: true, handshake: %{status: :finished}} = state
      ) do
    actions =
      maybe_send_stream_format(ctx) ++ [event: {pad, state.handshake.keying_material_event}]

    {actions, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, @component_id), ctx, state) do
    {maybe_send_stream_format(ctx), state}
  end

  @impl true
  def handle_write(
        Pad.ref(:input, @component_id) = pad,
        %Membrane.Buffer{payload: payload, metadata: metadata},
        _ctx,
        %{selected_alloc: alloc} = state
      )
      when is_pid(alloc) do
    send_ice_payload(alloc, payload, state.telemetry_label, Map.get(metadata, :timestamp))
    {[demand: pad], state}
  end

  @impl true
  def handle_event(
        Pad.ref(:input, @component_id) = pad,
        %Funnel.NewInputEvent{},
        _ctx,
        %{dtls?: true, handshake: %{status: :finished}} = state
      ) do
    {[event: {pad, state.handshake.keying_material_event}], state}
  end

  @impl true
  def handle_event(_pad, _event, _ctx, state), do: {[], state}

  @impl true
  def handle_tick(:keepalive_timer, _ctx, state) do
    with %{selected_alloc: alloc_pid} when is_pid(alloc_pid) <- state,
         %{^alloc_pid => %{magic: magic}} when magic != nil <- state.turn_allocs do
      tr_id = Utils.generate_transaction_id()
      Utils.send_binding_indication(alloc_pid, state.remote_ice_pwd, magic, tr_id)

      TelemetryMetrics.execute(@indication_sent_event, %{}, %{}, state.telemetry_label)

      Membrane.Logger.debug(
        "Sending Binding Indication with params: #{inspect(magic: magic, transaction_id: tr_id)}"
      )
    end

    {[], state}
  end

  # TODO Use mocking turn server instead of this
  @impl true
  def handle_parent_notification(:test_get_pid, _ctx, state) do
    msg = {:test_get_pid, self()}
    {[notify_parent: msg], state}
  end

  @impl true
  def handle_parent_notification(:gather_candidates, _ctx, state) do
    msg = {
      :new_candidate_full,
      Utils.generate_fake_ice_candidate({state.fake_candidate_ip, state.candidate_port})
    }

    {[notify_parent: msg], state}
  end

  @impl true
  def handle_parent_notification({:set_remote_credentials, credentials}, _ctx, state)
      when state.pending_connection_ready? do
    [_ice_ufrag, ice_pwd] = String.split(credentials)

    state =
      Map.merge(state, %{
        remote_ice_pwd: ice_pwd,
        sdp_offer_arrived?: true,
        connection_status_sent?: true,
        pending_connection_ready?: false
      })
      |> stop_ice_restart_timer()

    Membrane.OpenTelemetry.add_event(@life_span_id, :component_ready)
    actions = [notify_parent: {:connection_ready, @stream_id, @component_id}]
    {actions, state}
  end

  @impl true
  def handle_parent_notification({:set_remote_credentials, credentials}, _ctx, state) do
    [_ice_ufrag, ice_pwd] = String.split(credentials)

    state =
      Map.merge(state, %{
        remote_ice_pwd: ice_pwd,
        sdp_offer_arrived?: true
      })

    {[], state}
  end

  @impl true
  def handle_parent_notification(:restart_stream, _ctx, state) do
    ice_ufrag = Utils.generate_ice_ufrag()
    ice_pwd = Utils.generate_ice_pwd()

    state =
      Map.merge(state, %{
        local_ice_pwd: ice_pwd,
        connection_status_sent?: false,
        sdp_offer_arrived?: false
      })
      |> start_ice_restart_timer()

    Membrane.OpenTelemetry.add_event(@life_span_id, :restart_stream)

    credentials = "#{ice_ufrag} #{ice_pwd}"
    {[notify_parent: {:local_credentials, credentials}], state}
  end

  @impl true
  def handle_parent_notification(:peer_candidate_gathering_done, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_info({:failed_to_send_pkt, error, pkt_size}, _ctx, state) do
    Membrane.Logger.warn("ICE failed to send #{pkt_size} bytes due to socket error: #{error}")

    Membrane.TelemetryMetrics.execute(
      @send_error_event,
      %{bytes: pkt_size},
      %{},
      state.telemetry_label
    )

    {[], state}
  end

  @impl true
  def handle_info({:alloc_deleting, alloc_pid}, _ctx, state) do
    Membrane.Logger.debug("Deleting allocation with pid #{inspect(alloc_pid)}")
    {_alloc, state} = pop_in(state, [:turn_allocs, alloc_pid])
    {[], state}
  end

  @impl true
  def handle_info(
        {:connectivity_check, attrs, alloc_pid},
        ctx,
        state
      ) do
    state =
      if Map.has_key?(state.turn_allocs, alloc_pid) do
        state
      else
        Membrane.Logger.debug(
          "First connectivity check arrived from allocation with pid #{inspect(alloc_pid)}"
        )

        Process.monitor(alloc_pid)

        span_id = alloc_span_id(alloc_pid)

        Membrane.OpenTelemetry.start_span(span_id,
          name: @alloc_span_name,
          parent_id: @life_span_id
        )

        Membrane.OpenTelemetry.set_attribute(span_id, :pid, inspect(alloc_pid))

        put_in(state, [:turn_allocs, alloc_pid], %Allocation{pid: alloc_pid})
      end

    {state, actions} = do_handle_connectivity_check(Map.new(attrs), alloc_pid, ctx, state)
    {actions, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, _process, alloc_pid, _reason}, _ctx, state) do
    alloc_span_id(alloc_pid)
    |> Membrane.OpenTelemetry.end_span()

    {[], state}
  end

  @impl true
  def handle_info({:ice_payload, payload, timestamp}, ctx, state) do
    TelemetryMetrics.execute(
      @payload_received_event,
      %{bytes: byte_size(payload)},
      %{},
      state.telemetry_label
    )

    if state.dtls? and Utils.is_dtls_hsk_packet(payload) do
      state =
        if state.first_dtls_hsk_packet_arrived do
          state
        else
          Membrane.OpenTelemetry.start_span(@dtls_handshake_span_id, parent_id: @life_span_id)
          %{state | first_dtls_hsk_packet_arrived: true}
        end

      ExDTLS.process(state.handshake.state.dtls, payload)
      |> handle_process_result(ctx, state)
    else
      out_pad = Pad.ref(:output, @component_id)

      actions =
        cond do
          not Map.has_key?(ctx.pads, out_pad) ->
            Membrane.Logger.warn(
              "No links for component: #{@component_id}. Ignoring incoming message."
            )

            []

          ctx.playback != :playing ->
            Membrane.Logger.debug(
              "Received message in playback state: #{ctx.playback}. Ignoring."
            )

            []

          true ->
            [
              buffer:
                {out_pad, %Membrane.Buffer{payload: payload, metadata: %{timestamp: timestamp}}}
            ]
        end

      {actions, state}
    end
  end

  @impl true
  def handle_info({:retransmit, _dtls_pid, packets}, ctx, state) do
    # Treat retransmitted packets in the same way as regular handshake_packets
    handle_process_result({:handshake_packets, packets}, ctx, state)
  end

  @impl true
  def handle_info(:ice_restart_timeout, _ctx, state) do
    Membrane.Logger.debug("ICE restart failed due to timeout")
    Membrane.OpenTelemetry.add_event(@life_span_id, :ice_restart_timeout)

    state = %{state | connection_status_sent?: true, pending_connection_ready?: false}
    actions = [notify_parent: {:connection_failed, @stream_id, @component_id}]
    {actions, state}
  end

  @impl true
  def handle_info(msg, _ctx, state) do
    Membrane.Logger.warn("Received unknown message: #{inspect(msg)}")
    {[], state}
  end

  defp do_handle_connectivity_check(%{class: :request} = attrs, alloc_pid, ctx, state) do
    log_debug_connectivity_check(attrs)

    TelemetryMetrics.execute(@request_received_event, %{}, %{}, state.telemetry_label)

    alloc_span_id(alloc_pid)
    |> Membrane.OpenTelemetry.add_event(:binding_request_received,
      allocation: inspect(alloc_pid),
      use_candidate: attrs.use_candidate
    )

    alloc = state.turn_allocs[alloc_pid]

    Utils.send_binding_success(
      alloc_pid,
      state.local_ice_pwd,
      attrs.magic,
      attrs.trid,
      attrs.username
    )

    TelemetryMetrics.execute(@response_sent_event, %{}, %{}, state.telemetry_label)

    [magic: attrs.magic, transaction_id: attrs.trid, username: attrs.username]
    |> then(&"Sending Binding Success with params: #{inspect(&1)}")
    |> Membrane.Logger.debug()

    alloc = %Allocation{alloc | magic: attrs.magic}

    alloc =
      if attrs.use_candidate,
        do: %Allocation{alloc | in_nominated_pair: true},
        else: alloc

    state = put_in(state, [:turn_allocs, alloc_pid], alloc)
    maybe_select_alloc(alloc, ctx, state)
  end

  defp do_handle_connectivity_check(attrs, _alloc_pid, _ctx, state) do
    log_debug_connectivity_check(attrs)
    {state, []}
  end

  defp log_debug_connectivity_check(attrs) do
    request_type =
      case attrs.class do
        :response -> "Success"
        :request -> "Request"
        :error -> "Error"
        :indication -> "Indication"
      end

    Map.delete(attrs, :class)
    |> Map.to_list()
    |> then(&"Received Binding #{request_type} with params: #{inspect(&1)}")
    |> Membrane.Logger.debug()
  end

  defp maybe_select_alloc(alloc, ctx, state) do
    if alloc.in_nominated_pair and alloc.pid != state.selected_alloc do
      select_alloc(alloc.pid, ctx, state)
    else
      {state, []}
    end
  end

  defp select_alloc(alloc_pid, ctx, state) do
    state = Map.put(state, :selected_alloc, alloc_pid)
    Membrane.Logger.debug("Component #{@component_id} READY")

    Membrane.OpenTelemetry.add_event(@life_span_id, :new_selected_allocation,
      allocation: inspect(alloc_pid)
    )

    alloc_span_id(alloc_pid)
    |> Membrane.OpenTelemetry.add_event(:allocation_selected)

    state = %{state | component_connected?: true}

    {state, actions} =
      if state.dtls? == false or state.handshake.status == :finished do
        maybe_send_connection_ready(state)
      else
        Membrane.Logger.debug("Checking for cached handshake packets")

        if state.cached_hsk_packets == nil do
          Membrane.Logger.debug("Nothing to be sent for component: #{@component_id}")
        else
          Membrane.Logger.debug(
            "Sending cached handshake packets for component: #{@component_id}"
          )

          send_ice_payload(
            state.selected_alloc,
            state.cached_hsk_packets,
            state.telemetry_label
          )
        end

        with %{dtls?: true} <- state, %{dtls: dtls, client_mode: true} <- state.handshake.state do
          {:ok, packets} = ExDTLS.do_handshake(dtls)
          send_ice_payload(state.selected_alloc, packets, state.telemetry_label)
        else
          _state -> :ok
        end

        {state, actions} =
          if state.handshake.status == :finished do
            maybe_send_connection_ready(state)
          else
            {state, []}
          end

        {%{state | cached_hsk_packets: nil}, actions}
      end

    {state, demand_actions} = handle_component_state_ready(ctx, state)
    actions = demand_actions ++ actions
    {state, actions}
  end

  defp handle_process_result(:handshake_want_read, _ctx, state) do
    {[], state}
  end

  defp handle_process_result({:ok, _packets}, _ctx, state) do
    Membrane.Logger.warn("Got regular handshake packet. Ignoring for now.")
    {[], state}
  end

  defp handle_process_result({:handshake_packets, packets}, _ctx, state) do
    if state.component_connected? do
      send_ice_payload(state.selected_alloc, packets, state.telemetry_label)
      {[], state}
    else
      # if connection is not ready yet cache data
      # TODO maybe try to send?
      state = %{state | cached_hsk_packets: packets}
      {[], state}
    end
  end

  defp handle_process_result({:handshake_finished, hsk_data}, ctx, state),
    do: handle_end_of_hsk(hsk_data, ctx, state)

  defp handle_process_result({:handshake_finished, hsk_data, packets}, ctx, state) do
    send_ice_payload(state.selected_alloc, packets, state.telemetry_label)
    handle_end_of_hsk(hsk_data, ctx, state)
  end

  defp handle_process_result({:connection_closed, reason}, _ctx, state) do
    Membrane.Logger.debug("Connection closed, reason: #{inspect(reason)}. Ignoring for now.")
    {[], state}
  end

  defp handle_end_of_hsk(hsk_data, ctx, state) do
    Membrane.OpenTelemetry.end_span(@dtls_handshake_span_id)

    hsk_state = state.handshake.state
    event = to_srtp_keying_material_event(hsk_data)

    state =
      Map.put(state, :handshake, %{
        state: hsk_state,
        status: :finished,
        keying_material_event: event
      })

    {state, connection_ready_actions} = maybe_send_connection_ready(state)

    actions =
      connection_ready_actions ++
        maybe_send_demands_actions(ctx, state) ++
        maybe_send_keying_material_to_output(ctx, state)

    {actions, state}
  end

  defp handle_component_state_ready(ctx, state) do
    state = %{state | component_ready?: true}
    actions = maybe_send_demands_actions(ctx, state)
    {state, actions}
  end

  defp maybe_send_demands_actions(ctx, state) do
    pad = Pad.ref(:input, @component_id)
    # if something is linked, component is ready and handshake is done then send demands
    if Map.has_key?(ctx.pads, pad) and state.component_ready? and
         state.handshake.status == :finished do
      event = if state.dtls?, do: [event: {pad, state.handshake.keying_material_event}], else: []
      event ++ [demand: pad]
    else
      []
    end
  end

  defp maybe_send_keying_material_to_output(ctx, state) do
    pad = Pad.ref(:output, @component_id)

    if Map.has_key?(ctx.pads, pad),
      do: [event: {pad, state.handshake.keying_material_event}],
      else: []
  end

  defp maybe_send_stream_format(ctx) do
    pad = Pad.ref(:output, @component_id)

    if ctx.playback == :playing do
      [stream_format: {pad, %RemoteStream{}}]
    else
      []
    end
  end

  defp start_ice_restart_timer(state) do
    timer_ref = Process.send_after(self(), :ice_restart_timeout, @ice_restart_timeout)
    %{state | ice_restart_timer: timer_ref}
  end

  defp stop_ice_restart_timer(%{ice_restart_timer: timer_ref} = state)
       when is_reference(timer_ref) do
    Process.cancel_timer(timer_ref)
    state
  end

  defp stop_ice_restart_timer(state), do: state

  defp maybe_send_connection_ready(
         %{connection_status_sent?: false, sdp_offer_arrived?: true} = state
       ) do
    state =
      %{state | connection_status_sent?: true}
      |> stop_ice_restart_timer()

    Membrane.OpenTelemetry.add_event(@life_span_id, :component_state_ready)
    actions = [notify_parent: {:connection_ready, @stream_id, @component_id}]

    {state, actions}
  end

  defp maybe_send_connection_ready(
         %{connection_status_sent?: false, sdp_offer_arrived?: false} = state
       ),
       do: {%{state | pending_connection_ready?: true}, []}

  defp maybe_send_connection_ready(state), do: {state, []}

  defp to_srtp_keying_material_event(handshake_data) do
    {local_keying_material, remote_keying_material, protection_profile} = handshake_data

    %SRTP.KeyingMaterialEvent{
      local_keying_material: local_keying_material,
      remote_keying_material: remote_keying_material,
      protection_profile: protection_profile
    }
  end

  defp send_ice_payload(alloc_pid, payload, telemetry_label, timestamp \\ nil) do
    TelemetryMetrics.execute(
      @payload_sent_event,
      %{bytes: byte_size(payload)},
      %{},
      telemetry_label
    )

    if timestamp do
      processing_time =
        (:erlang.monotonic_time() - timestamp) |> System.convert_time_unit(:native, :microsecond)

      TelemetryMetrics.execute(
        @buffer_processing_time,
        %{microseconds: processing_time},
        %{},
        telemetry_label
      )
    end

    send(alloc_pid, {:send_ice_payload, payload})
  end

  # defp alloc_span_id(alloc_pid), do: "alloc_span:#{inspect(alloc_pid)}"
  defp alloc_span_id(alloc_pid), do: {:turn_allocation_span, alloc_pid}
end
