defmodule Membrane.RTC.Engine.SpeakersDetector do
  @moduledoc false

  use GenServer
  alias Membrane.RTC.Engine.EndpointManager

  @type options_t :: [
          ets_name: atom(),
          track_manager: pid()
        ]

  @spec start_link(opts :: options_t()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    {:ok,
     %{
       ets_name: opts[:ets_name],
       track_manager: opts[:track_manager],
       ends_of_speach: %{}
     }}
  end

  @impl true
  def handle_info({:calculcate_track_priorities, notifications, endpoint_managers}, state) do
    ends_of_speach =
      Enum.reduce(notifications, state.ends_of_speach, fn
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
      ends_of_speach
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

    update_ets(track_priorities, all_video_tracks, state.ets_name)

    send(state.track_manager, {:track_priorities, track_priorities})

    {:noreply, %{state | ends_of_speach: ends_of_speach}}
  end

  defp update_ets(endpoint_name_to_tracks, all_video_tracks, ets_name) do
    track_id_to_endpoints = Map.new(all_video_tracks, &{&1.id, []})

    track_id_to_endpoints =
      for {endpoint_name, tracks} <- endpoint_name_to_tracks,
          received_track <- tracks,
          reduce: track_id_to_endpoints do
        acc ->
          Map.update(acc, received_track, [endpoint_name], &[endpoint_name | &1])
      end

    Enum.map(track_id_to_endpoints, fn {track_id, endpoints} ->
      :ets.insert(ets_name, {track_id, endpoints})
    end)
  end
end
