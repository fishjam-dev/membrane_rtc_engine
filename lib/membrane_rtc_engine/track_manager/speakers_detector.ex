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
       endpoint_name_to_end_of_speech: %{}
     }}
  end

  @impl true
  def handle_info({:vad, notifications, endpoints}, state) do
    endpoint_name_to_end_of_speech =
      Enum.reduce(notifications, state.endpoint_name_to_end_of_speech, fn
        {:speech, endpoint_name}, acc ->
          Map.put(acc, endpoint_name, :speaking)

        {:silence, endpoint_name, timestamp}, acc ->
          Map.put(acc, endpoint_name, timestamp)

        {:register, endpoint_name}, acc ->
          Map.put_new(acc, endpoint_name, nil)

        _other, acc ->
          acc
      end)

    current_time = System.monotonic_time()

    ordered_endpoints_name =
      endpoint_name_to_end_of_speech
      |> Enum.map(fn
        {endpoint_name, :speaking} -> {endpoint_name, 1}
        {endpoint_name, nil} -> {endpoint_name, 0}
        {endpoint_name, timestamp} -> {endpoint_name, timestamp / current_time}
      end)
      |> Enum.sort_by(fn {_endpoint_name, time} -> time end)
      |> Enum.map(fn {endpoint_name, _time} -> endpoint_name end)

    ordered_tracks = calculate_new_tracks_priority(endpoints, ordered_endpoints_name)

    endpoints = Map.values(endpoints)

    endpoint_name_to_tracks = endpoint_to_receive_tracks(ordered_tracks, endpoints)

    all_video_tracks = Enum.flat_map(endpoints, &EndpointManager.get_video_tracks/1)

    insert_tracks_priority_per_track(endpoint_name_to_tracks, all_video_tracks, state.ets_name)

    send(state.track_manager, {:vad_response, endpoint_name_to_tracks})

    {:noreply, %{state | endpoint_name_to_end_of_speech: endpoint_name_to_end_of_speech}}
  end

  defp calculate_new_tracks_priority(endpoints, ordered_endpoints_name) do
    Enum.flat_map(ordered_endpoints_name, fn endpoint_name ->
      endpoints |> Map.get(endpoint_name) |> EndpointManager.get_video_tracks()
    end)
  end

  defp endpoint_to_receive_tracks(ordered_tracks, endpoints) do
    for endpoint <- endpoints,
        received_track <- EndpointManager.calculate_tracks_priority(endpoint, ordered_tracks),
        reduce: %{} do
      acc -> Map.update(acc, endpoint.id, [received_track], &[received_track | &1])
    end
  end

  defp insert_tracks_priority_per_track(endpoint_name_to_tracks, all_video_tracks, ets_name) do
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
