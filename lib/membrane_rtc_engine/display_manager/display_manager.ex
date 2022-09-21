defmodule Membrane.RTC.Engine.DisplayManager do
  @moduledoc false

  # A GenServer responsible for deciding which track is sent to which peers.
  # It makes a decision based on metrics, which currently include only VAD notifications.
  # To calculate metrics DisplayManager needs to have a copy of the state of the room.
  # Each endpoints state is represented as EndpointManager. After calculating metrics it takes into account
  # client preferences puts results in the ETS table and sends a message to the RTC Engine that tees should update to whom they send buffers.

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.EndpointManager

  @enforce_keys [:video_tracks_limit, :endpoint_name]

  defstruct @enforce_keys ++ [endpoint_managers: %{}, ends_of_speech: %{}]

  @spec new :: %Membrane.RTC.Engine.DisplayManager{endpoint_managers: %{}}
  def new(endpoint_name, video_tracks_limit \\ nil) do
    %__MODULE__{endpoint_name: endpoint_name, video_tracks_limit: video_tracks_limit}
  end

  def add_vad_notification(state, {:vad_notification, endpoint_name, value, timestamp})
      when value in [:silence, :speech] do
    calculate_tracks_priority(state, {value, endpoint_name, timestamp})
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
    state = calculate_tracks_priority(state, {:unregister, endpoint_name})

    {:noreply, state}
  end

  def add_inbound_tracks(state, tracks) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        state.endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :inbound_tracks)
      )

    %{state | endpoint_managers: endpoints}
  end

  def subscribe_tracks(state, tracks) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        state.endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :outbound_tracks)
      )

    state = %{state | endpoint_managers: endpoints}

    calculate_tracks_priority(state)
  end

  def remove_track(state, tracks) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        state.endpoint_name,
        &EndpointManager.remove_tracks(&1, tracks, :inbound_tracks)
      )

    %{state | endpoint_managers: endpoints}
  end

  def prioritize_track(state, track_id) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        state.endpoint_name,
        &EndpointManager.prioritize_track(&1, track_id)
      )

    state = %{state | endpoint_managers: endpoints}

    calculate_tracks_priority(state)
  end

  def unprioritize_track(state, track_id) do
    endpoints =
      Map.update!(
        state.endpoint_managers,
        state.endpoint_name,
        &EndpointManager.unprioritize_track(&1, track_id)
      )

    %{state | endpoint_managers: endpoints}
  end

  def change_prefered_video_sizes(state, big_screens, medium_screens, small_screens, same_size?) do
    endpoints =
      update_in(
        state.endpoint_managers,
        [state.endpoint_name, :screen_sizes],
        &%{
          &1
          | same_size?: same_size?,
            big_screens: big_screens,
            medium_screens: medium_screens,
            small_screens: small_screens
        }
      )

    %{state | endpoint_managers: endpoints}
  end

  defp calculate_tracks_priority(state, vad_notification \\ nil) do
    ends_of_speech =
      case vad_notification do
        {vad_value, endpoint_name, timestamp} ->
          Map.put(state.ends_of_speech, endpoint_name, {vad_value, timestamp})

        {:new_track, endpoint_name} ->
          Map.put_new(state.ends_of_speech, endpoint_name, nil)

        {:unregister, endpoint_name} ->
          Map.delete(state.ends_of_speech, endpoint_name)

        nil ->
          state.ends_of_speech
      end

    ordered_endpoint_names =
      ends_of_speech
      |> Enum.sort_by(fn {_endpoint_name, time} -> time end, &sort_vads/2)
      |> Enum.map(fn {endpoint_name, _time} -> endpoint_name end)

    ordered_tracks =
      Enum.flat_map(ordered_endpoint_names, fn endpoint_name ->
        state.endpoint_managers
        |> Map.get(endpoint_name)
        |> EndpointManager.get_inbound_video_tracks()
      end)

    endpoint_managers = Map.values(state.endpoint_managers)

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
  end

  defp sort_vads(_vad, nil), do: true
  defp sort_vads(nil, _vad), do: false
  defp sort_vads({:silence, timestamp1}, {:silence, timestamp2}), do: timestamp1 > timestamp2
  defp sort_vads({:silence, _timestamp1}, {:speech, _timestamp2}), do: false
  defp sort_vads({:speech, _timestamp1}, {:silence, _timestamp2}), do: true
  defp sort_vads({:speech, timestamp1}, {:speech, timestamp2}), do: timestamp1 < timestamp2
end
