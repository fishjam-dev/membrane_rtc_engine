defmodule TestVideoroomWeb.RoomChannel do
  use TestVideoroomWeb, :channel

  require Logger

  @impl true
  def join("room" = room_id, _params, socket) do
    case :global.whereis_name(room_id) do
      :undefined -> TestVideoroom.Room.start(name: {:global, room_id})
      pid -> {:ok, pid}
    end
    |> case do
      {:ok, room} ->
        join_room(room_id, room, socket)

      {:error, {:already_started, _pid}} ->
        room = :global.whereis_name(room_id)
        join_room(room_id, room, socket)

      {:error, reason} ->
        Logger.error("""
        Failed to start room.
        Room: #{inspect(room_id)}
        Reason: #{inspect(reason)}
        """)

        {:error, %{reason: "failed to start room"}}
    end
  end

  defp join_room(room_id, room, socket) do
    peer_id = "#{UUID.uuid4()}"
    Process.monitor(room)
    TestVideoroom.Room.add_peer_channel(room, self(), peer_id)
    {:ok, Phoenix.Socket.assign(socket, %{room_id: room_id, room: room, peer_id: peer_id})}
  end

  @impl true
  def handle_in("mediaEvent", string, socket) do
    {:ok, bin} = Base.decode16(string, case: :lower)
    send(socket.assigns.room, {:media_event, socket.assigns.peer_id, bin})

    {:noreply, socket}
  end

  @impl true
  def handle_info({:media_event, event}, socket) do
    event = Base.encode16(event, case: :lower)
    push(socket, "mediaEvent", %{data: event})

    {:noreply, socket}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, room, :normal}, %{assigns: %{room: room}} = state) do
    {:stop, :normal, state}
  end
end
