defmodule Videoroom.Room do
  @moduledoc false

  use GenServer

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.ExWebRTC
  alias Membrane.RTC.Engine.Message.EndpointMessage

  def start(init_arg, opts) do
    GenServer.start(__MODULE__, init_arg, opts)
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  @impl true
  def init(room_id) do
    Membrane.Logger.info("Spawning room proces: #{inspect(self())}")

    rtc_engine_options = [id: room_id]
    ice_port_range = Application.fetch_env!(:membrane_videoroom_demo, :ice_port_range)

    {:ok, pid} = Membrane.RTC.Engine.start(rtc_engine_options, [])
    Engine.register(pid, self())
    Process.monitor(pid)

    {:ok, %{rtc_engine: pid, peer_channels: %{}, ice_port_range: ice_port_range}}
  end

  @impl true
  def handle_info({:add_peer_channel, peer_channel_pid, peer_id}, state) do
    Process.monitor(peer_channel_pid)

    webrtc_endpoint = %ExWebRTC{
      rtc_engine: state.rtc_engine,
      ice_port_range: state.ice_port_range
    }

    :ok = Engine.add_endpoint(state.rtc_engine, webrtc_endpoint, id: peer_id)

    {:noreply, put_in(state, [:peer_channels, peer_id], peer_channel_pid)}
  end

  @impl true
  def handle_info(%EndpointMessage{endpoint_id: to, message: {:media_event, data}}, state) do
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
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    case state do
      %{rtc_engine: ^pid} ->
        {:stop, :normal, state}

      %{rtc_engine: rtc_engine, peer_channels: peer_channels} ->
        {peer_id, _peer_channel_id} =
          Enum.find(peer_channels, fn {_peer_id, peer_channel_pid} ->
            peer_channel_pid == pid
          end)

        Engine.remove_endpoint(rtc_engine, peer_id)

        {:noreply, state}
    end
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
