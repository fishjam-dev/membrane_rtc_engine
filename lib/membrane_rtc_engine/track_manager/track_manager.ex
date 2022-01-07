defmodule Membrane.RTC.Engine.TrackManager do
  @moduledoc """
  TBD
  """
  use GenServer
  alias Membrane.RTC.Engine.EndpointManager

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(ets_name) do
    ets_name = :"#{ets_name}"
    :ets.new(ets_name, [:set, :public, :named_table])
    {:ok, %{silence_stack: [], speech_stack: [], endpoints: %{}, ets_name: ets_name}}
  end

  @impl true
  def handle_info({:vad_notification, audio_track_id, :speech}, state) do
    {silence_stack, speech_stack} =
      move_from_stack_to_stack(state.silence_stack, state.speech_stack, audio_track_id)

    ordered_tracks = calculate_new_tracks_priority(state.endpoints, speech_stack ++ silence_stack)

    insert_tracks_priority_per_track(ordered_tracks, Map.values(state.endpoints), state.ets_name)

    {:noreply, %{state | silence_stack: silence_stack, speech_stack: speech_stack}}
  end

  @impl true
  def handle_info({:vad_notification, audio_track_id, :silence}, state) do
    {speech_stack, silence_stack} =
      move_from_stack_to_stack(state.speech_stack, state.silence_stack, audio_track_id)

    {:noreply, %{state | silence_stack: silence_stack, speech_stack: speech_stack}}
  end

  @impl true
  def handle_info({:register_endpoint, endpoint_name, video_tracks_limit}, state) do
    endpoints =
      Map.put(
        state.endpoints,
        endpoint_name,
        EndpointManager.new(endpoint_name, video_tracks_limit)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info({:new_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :inbound_tracks)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info({:subscribe_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :outbound_tracks)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info({:removed_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.remove_tracks(&1, tracks, :inbound_tracks)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  defp calculate_new_tracks_priority(endpoints, ordered_tracks) do
    for audio_track <- ordered_tracks,
        {_endpoint_id, endpoint} <- endpoints,
        video_track <- EndpointManager.map_audio_to_video(endpoint, audio_track),
        do: video_track
  end

  defp move_from_stack_to_stack(from_stack, to_stack, track_id) do
    from_stack = Enum.reject(from_stack, &(&1 == track_id))
    to_stack = [track_id | to_stack]
    {from_stack, to_stack}
  end

  defp insert_tracks_priority_per_track(ordered_tracks, endpoints, ets_name) do
    track_id_to_endpoints =
      for endpoint <- endpoints,
          received_track <- EndpointManager.calculate_tracks_priority(endpoint, ordered_tracks),
          reduce: %{} do
        acc -> Map.update(acc, received_track, [endpoint.id], &[endpoint.id | &1])
      end

    Enum.map(track_id_to_endpoints, fn {track_id, endpoints} ->
      :ets.insert(ets_name, {track_id, endpoints})
    end)
  end
end
