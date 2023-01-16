defmodule TestVideoroom.Room do
  @moduledoc false

  use GenServer

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Message
  alias Membrane.RTC.Engine.Endpoint.WebRTC
  alias Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastConfig
  alias Membrane.WebRTC.Extension.{Mid, Rid, TWCC}
  require Logger

  def start(opts) do
    GenServer.start(__MODULE__, [], opts)
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def add_peer_channel(room, peer_channel_pid, peer_id) do
    GenServer.call(room, {:add_peer_channel, peer_channel_pid, peer_id})
  end

  def register_new_peer_listener(room, listener) do
    GenServer.call(room, {:register_new_peer_listener, listener})
  end

  @impl true
  def init(room_id) do
    Logger.info("Spawning room process: #{inspect(self())}")

    turn_mock_ip = Application.fetch_env!(:test_videoroom, :integrated_turn_ip)
    turn_ip = if Mix.env() == :prod, do: {0, 0, 0, 0}, else: turn_mock_ip

    turn_cert_file =
      case Application.fetch_env(:test_videoroom, :integrated_turn_cert_pkey) do
        {:ok, val} -> val
        :error -> nil
      end

    integrated_turn_options = [
      ip: turn_ip,
      mock_ip: turn_mock_ip,
      ports_range: Application.fetch_env!(:test_videoroom, :integrated_turn_port_range),
      cert_file: turn_cert_file
    ]

    network_options = [
      integrated_turn_options: integrated_turn_options,
      integrated_turn_domain: Application.fetch_env!(:test_videoroom, :integrated_turn_domain),
      dtls_pkey: Application.get_env(:test_videoroom, :dtls_pkey),
      dtls_cert: Application.get_env(:test_videoroom, :dtls_cert)
    ]

    rtc_engine_options = [
      id: room_id
    ]

    {:ok, pid} = Membrane.RTC.Engine.start(rtc_engine_options, [])
    Process.monitor(pid)
    Engine.register(pid, self())

    {:ok,
     %{
       rtc_engine: pid,
       peer_channels: %{},
       network_options: network_options,
       listeners: [],
       room_id: room_id
     }}
  end

  @impl true
  def handle_call({:register_new_peer_listener, listener}, _from, state) do
    {:reply, :ok, %{state | listeners: [listener | state.listeners]}}
  end

  @impl true
  def handle_call({:add_peer_channel, peer_channel_pid, peer_id}, _from, state) do
    state = put_in(state, [:peer_channels, peer_id], peer_channel_pid)
    Process.monitor(peer_channel_pid)

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
      rtc_engine: state.rtc_engine,
      ice_name: peer_id,
      extensions: %{},
      owner: self(),
      integrated_turn_options: state.network_options[:integrated_turn_options],
      integrated_turn_domain: state.network_options[:integrated_turn_domain],
      handshake_opts: handshake_opts,
      log_metadata: [peer_id: peer_id],
      telemetry_label: [room_id: state.room_id, peer_id: peer_id],
      webrtc_extensions: [Mid, Rid, TWCC],
      simulcast_config: %SimulcastConfig{
        enabled: true,
        initial_target_variant: fn _track -> :medium end
      }
    }

    :ok = Engine.add_endpoint(state.rtc_engine, endpoint, peer_id: peer_id)

    for listener <- state.listeners do
      send(listener, {:room, :new_peer})
    end

    {:reply, :ok, state}
  end

  @impl true
  def handle_info(%Message.EndpointMessage{endpoint_id: to, message: {:media_event, data}}, state) do
    if state.peer_channels[to] != nil do
      send(state.peer_channels[to], {:media_event, data})
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:media_event, from, event}, state) do
    Engine.message_endpoint(state.rtc_engine, from, {:media_event, event})
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, sfu_pid, _reason}, %{sfu_engine: sfu_pid} = state) do
    {:stop, "rtc engine down", state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    {peer_id, _peer_channel_id} =
      state.peer_channels
      |> Enum.find(fn {_peer_id, peer_channel_pid} -> peer_channel_pid == pid end)

    Engine.remove_endpoint(state.rtc_engine, peer_id)
    {_elem, state} = pop_in(state, [:peer_channels, peer_id])
    {:noreply, state}
  end
end
