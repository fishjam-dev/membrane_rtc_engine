defmodule Membrane.RTC.Engine.Network do
  use GenServer

  alias Membrane.RTC.Engine

  @ingress_node :"ingress@Felikss-MacBook-Pro"
  @egress_node :"egress@Felikss-MacBook-Pro"

  @doc """
  Registers process with pid `who` for receiving messages from RTC Engine
  """
  @spec register(rtc_engine :: pid(), who :: pid()) :: :ok
  def register(rtc_engine, who \\ self()) do
    send(rtc_engine, {:register, who})
    :ok
  end

  @doc """
  Unregisters process with pid `who` from receiving messages from RTC Engine
  """
  @spec unregister(rtc_engine :: pid(), who :: pid()) :: :ok
  def unregister(rtc_engine, who \\ self()) do
    send(rtc_engine, {:unregister, who})
    :ok
  end

  @impl true
  def init(params) do
    room_id = params[:room_id]
    trace_ctx = params[:trace_ctx]
    network_options = params[:network_options]

    {:ok, ingress_engine} =
      start_engine(
        "ingress:#{room_id}",
        room_id,
        @ingress_node,
        @egress_node,
        Endpoint.DistributedSink,
        trace_ctx
      )

    {:ok, egress_engine} =
      start_engine(
        "egress:#{room_id}",
        room_id,
        @egress_node,
        @ingress_node,
        Endpoint.DistributedSource,
        trace_ctx
      )

    {:ok,
     %{
       ingress_engine: ingress_engine,
       egress_engine: egress_engine,
       trace_ctx: trace_ctx,
       network_options: network_options
     }}
  end

  @impl true
  def handle_info(%Message.NewPeer{rtc_engine: rtc_engine, peer: peer}, state) do
    Membrane.Logger.info("New peer: #{inspect(peer)}. Accepting.")
    peer_channel_pid = Map.get(state.peer_channels, peer.id)
    peer_node = node(peer_channel_pid)

    handshake_opts =
      if state.network_options[:dtls_pkey] &&
           state.network_options[:dtls_cert] do
        [
          client_mode: false,
          dtls_srtp: true,
          pkey: state.network_options[:dtls_pkey],
          cert: state.network_options[:dtls_cert]
        ]
      else
        [
          client_mode: false,
          dtls_srtp: true
        ]
      end

    endpoint = %WebRTC{
      ice_name: peer.id,
      owner: self(),
      stun_servers: state.network_options[:stun_servers] || [],
      turn_servers: state.network_options[:turn_servers] || [],
      use_integrated_turn: state.network_options[:use_integrated_turn],
      integrated_turn_options: state.network_options[:integrated_turn_options],
      integrated_turn_domain: state.network_options[:integrated_turn_domain],
      handshake_opts: handshake_opts,
      log_metadata: [peer_id: peer.id],
      trace_context: state.trace_ctx
    }

    Engine.accept_peer(rtc_engine, peer.id)
    :ok = Engine.add_endpoint(rtc_engine, endpoint, peer_id: peer.id)

    {:noreply, state}
  end

  @impl true
  def handle_info(%Message.MediaEvent{} = message, state) do
    do_handle_message_from_node(message, state)
  end

  defp do_handle_message_from_node(message, state), do: {:ok, state}

  defp start_engine(id, room_id, node, twin_node, endpoint_module, trace_ctx) do
    {:ok, engine} = Engine.start(node, [id: id, trace_ctx: trace_ctx], [])

    endpoint =
      endpoint_module
      |> struct!(
        twin_node: twin_node,
        pair_id: room_id
      )

    Engine.register(engine, self())
    Endpoint.add_endpoint(engine, endpoint)

    {:ok, engine}
  end
end
