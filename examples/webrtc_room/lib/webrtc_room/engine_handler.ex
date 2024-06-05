defmodule WebRTCRoom.EngineHandler do
  use GenServer

  require Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Message.{EndpointAdded, EndpointMessage}

  @id_length 5

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def add_peer do
    GenServer.call(__MODULE__, :add_peer)
  end

  def forward_media_event(msg) do
    GenServer.call(__MODULE__, {:forward_media_event, msg})
  end

  @impl true
  def init(_) do
    {:ok, engine} = Engine.start_link([], [])
    :ok = Engine.register(engine)

    state = %{
      engine: engine,
      ids_to_tags: %{},
      pids_to_ids: %{},
      ids_to_pids: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:add_peer, {pid, tag}, state) do
    Process.monitor(pid)
    id = generate_id()
    pids_to_ids = Map.put(state.pids_to_ids, pid, id)
    ids_to_pids = Map.put(state.ids_to_pids, id, pid)
    ids_to_tags = Map.put(state.ids_to_tags, id, tag)

    endpoint = %Engine.Endpoint.ExWebRTC{rtc_engine: state.engine}
    :ok = Engine.add_endpoint(state.engine, endpoint, id: id)

    state = %{
      state
      | pids_to_ids: pids_to_ids,
        ids_to_pids: ids_to_pids,
        ids_to_tags: ids_to_tags
    }

    {:noreply, state}
  end

  @impl true
  def handle_call({:forward_media_event, event}, {pid, _}, state) do
    Logger.info("Received media event: #{inspect(event)}")

    id = Map.fetch!(state.pids_to_ids, pid)
    :ok = Engine.message_endpoint(state.engine, id, {:media_event, event})
    {:reply, :ok, state}
  end

  @impl true
  def handle_info(%EndpointAdded{endpoint_id: id}, state) do
    Logger.info("Added peer #{id}")

    {tag, ids_to_tags} = Map.pop!(state.ids_to_tags, id)
    pid = Map.fetch!(state.ids_to_pids, id)

    GenServer.reply({pid, tag}, :ok)

    state = %{state | ids_to_tags: ids_to_tags}
    {:noreply, state}
  end

  @impl true
  def handle_info(%EndpointMessage{endpoint_id: id, message: {:media_event, event}}, state) do
    Logger.info("Sending media event: #{inspect(event)}")

    pid = Map.fetch!(state.ids_to_pids, id)
    send(pid, {:media_event, event})
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.info("Received message: #{inspect(msg)}")

    {:noreply, state}
  end

  defp generate_id do
    @id_length
    |> :crypto.strong_rand_bytes()
    |> Base.encode16()
  end
end
