defmodule Membrane.RTC.Engine.SpeakersDetector do
  @moduledoc false

  use GenServer
  alias Membrane.RTC.Engine.EndpointManager

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    {:ok,
     %{
       ets_name: opts[:ets_name],
       track_manager: opts[:track_manager],
       silence_stack: [],
       speech_stack: []
     }}
  end

  @impl true
  def handle_info({:vad, notifications, endpoints}, state) do
    {speech_stack, silence_stack} =
      Enum.reduce(notifications, {state.speech_stack, state.silence_stack}, fn
        {:speech, track_id}, {speech_stack, silence_stack} ->
          {silence_stack, speech_stack} =
            move_from_stack_to_stack(silence_stack, speech_stack, track_id)

          {speech_stack, silence_stack}

        {:silence, track_id}, {speech_stack, silence_stack} ->
          {speech_stack, silence_stack} =
            move_from_stack_to_stack(speech_stack, silence_stack, track_id)

          {speech_stack, silence_stack}
      end)

    ordered_tracks = calculate_new_tracks_priority(endpoints, speech_stack ++ silence_stack)

    endpoint_name_to_tracks = endpoint_to_receive_tracks(ordered_tracks, Map.values(endpoints))

    insert_tracks_priority_per_track(endpoint_name_to_tracks, state.ets_name)

    send(state.track_manager, {:vad_response, endpoint_name_to_tracks})

    {:noreply, state}
  end

  defp move_from_stack_to_stack(from_stack, to_stack, track_id) do
    from_stack = Enum.reject(from_stack, &(&1 == track_id))
    to_stack = [track_id | to_stack]
    {from_stack, to_stack}
  end

  defp calculate_new_tracks_priority(endpoints, ordered_tracks) do
    for audio_track <- ordered_tracks,
        {_endpoint_id, endpoint} <- endpoints,
        video_track <- EndpointManager.map_audio_to_video(endpoint, audio_track),
        do: video_track
  end

  defp endpoint_to_receive_tracks(ordered_tracks, endpoints) do
    for endpoint <- endpoints,
        received_track <- EndpointManager.calculate_tracks_priority(endpoint, ordered_tracks),
        reduce: %{} do
      acc -> Map.update(acc, endpoint.id, [received_track], &[received_track | &1])
    end
  end

  defp insert_tracks_priority_per_track(endpoint_name_to_tracks, ets_name) do
    track_id_to_endpoints =
      for {endpoint_name, tracks} <- endpoint_name_to_tracks,
          received_track <- tracks,
          reduce: %{} do
        acc -> Map.update(acc, received_track, [endpoint_name], &[endpoint_name | &1])
      end

    Enum.map(track_id_to_endpoints, fn {track_id, endpoints} ->
      :ets.insert(ets_name, {track_id, endpoints})
    end)
  end
end
