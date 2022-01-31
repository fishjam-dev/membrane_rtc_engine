defmodule Membrane.RTC.Engine.DisplayManager do
  @moduledoc """
  A GenServer responsible for deciding which track is sent to which peers.

  It makes a decision based on metrics, which currently include only VAD notifications.
  To calculate metrics DisplayManager needs to have a copy of the state of the room.
  Each endpoints state is represented as EndpointManager. After calculating metrics it takes into account
  client preferences puts results in the ETS table and sends a message to the RTC Engine that tees should update to whom they send buffers.
  """
  use GenServer
  alias Membrane.RTC.Engine.EndpointManager

  @type options_t :: [
          ets_name: String.t(),
          engine: pid()
        ]

  @spec start_link(opts :: options_t()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    engine_pid = opts[:engine]
    ets_name = opts[:ets_name]
    ets_name = :"#{ets_name}"
    :ets.new(ets_name, [:set, :public, :named_table])
    display_manager = self()

    speakers_detector =
      Process.spawn(
        fn -> speakers_detector_loop(display_manager, ets_name) end,
        [:link]
      )

    {:ok,
     %{
       endpoint_managers: %{},
       ets_name: ets_name,
       engine: engine_pid,
       calculating_priority?: false,
       vads: [],
       speakers_detector: speakers_detector
     }}
  end

  @impl true
  def handle_info({:vad_notification, endpoint_name, :speech}, state) do
    state = maybe_calculate_priority(state, [{:speech, endpoint_name} | state.vads])
    {:noreply, state}
  end

  @impl true
  def handle_info({:vad_notification, endpoint_name, :silence}, state) do
    state = %{state | vads: [{:silence, endpoint_name, System.monotonic_time()} | state.vads]}
    {:noreply, state}
  end

  @impl true
  def handle_info({:track_priorities, endpoint_to_tracks}, state) do
    send(state.engine, {:track_priorities, endpoint_to_tracks})
    state = %{state | calculating_priority?: false}
    state = maybe_calculate_priority(state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:register_endpoint, endpoint_name, video_tracks_limit}, state) do
    endpoint_managers =
      Map.put(
        state.endpoint_managers,
        endpoint_name,
        EndpointManager.new(endpoint_name, video_tracks_limit)
      )

    {:noreply, %{state | endpoint_managers: endpoint_managers}}
  end

  @impl true
  def handle_info({:unregister_endpoint, endpoint_name}, state) do
    endpoints =
      Map.delete(
        state.endpoint_managers,
        endpoint_name
      )

    state = %{state | endpoint_managers: endpoints}
    state = maybe_calculate_priority(state, [{:unregister, endpoint_name} | state.vads])

    {:noreply, state}
  end

  @impl true
  def handle_info({:add_inbound_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :inbound_tracks)
      )

    state = %{state | endpoint_managers: endpoints}
    state = maybe_calculate_priority(state, [{:new_track, endpoint_name} | state.vads])

    {:noreply, state}
  end

  @impl true
  def handle_info({:subscribe_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :outbound_tracks)
      )

    {:noreply, %{state | endpoint_managers: endpoints}}
  end

  @impl true
  def handle_info({:removed_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        endpoint_name,
        &EndpointManager.remove_tracks(&1, tracks, :inbound_tracks)
      )

    {:noreply, %{state | endpoint_managers: endpoints}}
  end

  @impl true
  def handle_info({:prioritize_track, endpoint_name, track_id}, state) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        endpoint_name,
        &EndpointManager.prioritize_track(&1, track_id)
      )

    state = %{state | endpoint_managers: endpoints}

    state = maybe_calculate_priority(state, [:prioritize_track | state.vads])

    {:noreply, state}
  end

  @impl true
  def handle_info({:unprioritize_track, endpoint_name, track_id}, state) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        endpoint_name,
        &EndpointManager.unprioritize_track(&1, track_id)
      )

    {:noreply, %{state | endpoint_managers: endpoints}}
  end

  @impl true
  def handle_info(
        {:prefered_video_sizes, endpoint_name, big_screens, medium_screens, small_screens,
         same_size?},
        state
      ) do
    endpoints =
      update_in(
        state.endpoint_managers,
        [endpoint_name, :screen_sizes],
        &%{
          &1
          | same_size?: same_size?,
            big_screens: big_screens,
            medium_screens: medium_screens,
            small_screens: small_screens
        }
      )

    {:noreply, %{state | endpoint_managers: endpoints}}
  end

  defp maybe_calculate_priority(state, vads \\ nil) do
    vads = vads || state.vads

    vads = Enum.reverse(vads)

    if not state.calculating_priority? and Enum.count(vads) > 0 do
      send(state.speakers_detector, {:calculate_tracks_priority, vads, state.endpoint_managers})

      %{state | vads: [], calculating_priority?: true}
    else
      %{state | vads: vads}
    end
  end

  defp speakers_detector_loop(display_manager_pid, ets_name) do
    receive do
      {:calculate_tracks_priority, vads, endpoint_managers} ->
        track_priorities = calculate_tracks_priority(vads, endpoint_managers, ets_name)

        send(display_manager_pid, {:track_priorities, track_priorities})
        speakers_detector_loop(display_manager_pid, ets_name)
    end
  end

  defp calculate_tracks_priority(notifications, endpoint_managers, ets_name) do
    ends_of_speech =
      case :ets.lookup(ets_name, :ends_of_speech) do
        [{_key, ends_of_speech} | _] -> ends_of_speech
        [] -> %{}
      end

    ends_of_speech =
      Enum.reduce(notifications, ends_of_speech, fn
        {:speech, endpoint_name}, acc ->
          Map.put(acc, endpoint_name, :speaking)

        {:silence, endpoint_name, timestamp}, acc ->
          Map.put(acc, endpoint_name, timestamp)

        {:new_track, endpoint_name}, acc ->
          Map.put_new(acc, endpoint_name, nil)

        {:unregister, endpoint_name}, acc ->
          Map.delete(acc, endpoint_name)

        _other, acc ->
          acc
      end)

    current_time = System.monotonic_time()

    ordered_endpoint_names =
      ends_of_speech
      |> Enum.map(fn
        {endpoint_name, :speaking} -> {endpoint_name, 1}
        {endpoint_name, nil} -> {endpoint_name, 0}
        {endpoint_name, timestamp} -> {endpoint_name, timestamp / current_time}
      end)
      |> Enum.sort_by(fn {_endpoint_name, time} -> time end)
      |> Enum.map(fn {endpoint_name, _time} -> endpoint_name end)

    ordered_tracks =
      Enum.flat_map(ordered_endpoint_names, fn endpoint_name ->
        endpoint_managers |> Map.get(endpoint_name) |> EndpointManager.get_inbound_video_tracks()
      end)

    endpoint_managers = Map.values(endpoint_managers)

    track_priorities =
      for endpoint_manager <- endpoint_managers,
          endpoint_track_priorities <-
            EndpointManager.calculate_track_priorities(endpoint_manager, ordered_tracks),
          reduce: %{} do
        acc ->
          Map.update(
            acc,
            endpoint_manager.id,
            [endpoint_track_priorities],
            &[endpoint_track_priorities | &1]
          )
      end

    all_video_tracks =
      Enum.flat_map(endpoint_managers, &EndpointManager.get_inbound_video_tracks/1)

    update_ets(track_priorities, all_video_tracks, ets_name)

    :ets.insert(ets_name, {:ends_of_speech, ends_of_speech})

    track_priorities
  end

  defp update_ets(endpoint_name_to_tracks, all_video_tracks, ets_name) do
    empty_track_id_to_endpoints = Map.new(all_video_tracks, &{&1.id, []})

    track_id_to_endpoints =
      endpoint_name_to_tracks
      |> Enum.flat_map(fn {endpoint_name, tracks} -> Enum.map(tracks, &{endpoint_name, &1}) end)
      |> Enum.group_by(
        fn {_endpoint_name, track} -> track end,
        fn {endpoint_name, _track} -> endpoint_name end
      )
      |> Map.new()

    track_id_to_endpoints = Map.merge(empty_track_id_to_endpoints, track_id_to_endpoints)

    Enum.map(track_id_to_endpoints, fn {track_id, endpoints} ->
      :ets.insert(ets_name, {track_id, endpoints})
    end)
  end
end
