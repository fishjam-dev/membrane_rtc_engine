defmodule Membrane.RTC.Engine.TrackManager do
  @moduledoc """
  TBD
  """
  use GenServer
  alias Membrane.RTC.Engine.{SpeakersDetector, EndpointManager}

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    engine_pid = opts[:engine]
    ets_name = opts[:ets_name]
    ets_name = :"#{ets_name}"
    :ets.new(ets_name, [:set, :public, :named_table])
    {:ok, pid} = GenServer.start_link(SpeakersDetector, ets_name: ets_name, track_manager: self())

    {:ok,
     %{
       endpoints: %{},
       ets_name: ets_name,
       engine: engine_pid,
       speakers_detector: pid,
       calculating_priority?: false,
       vads: []
     }}
  end

  @impl true
  def handle_info({:vad_notification, audio_track_id, :speech}, state) do
    state =
      if state.calculating_priority? do
        %{state | vads: [{:speech, audio_track_id} | state.vads]}
      else
        send(state.speakers_detector, {:vad, state.vads, state.endpoints})
        %{state | vads: [], calculating_priority?: true}
      end

    {:noreply, state}
  end

  @impl true
  def handle_info({:vad_notification, audio_track_id, :silence}, state) do
    {:noreply, %{state | vads: [{:silence, audio_track_id} | state.vads]}}
  end

  @impl true
  def handle_info({:vad_response, endpoint_to_tracks}, state) do
    send(state.engine, {:tracks_priority, endpoint_to_tracks})
    {:noreply, state}
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

  @impl true
  def handle_info({:prioritize_track, endpoint_name, track_id}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.add_prioritized_track(&1, track_id)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info({:unprioritize_track, endpoint_name, track_id}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.remove_prioritized_track(&1, track_id)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info(
        {:prefered_video_sizes, endpoint_name, big_screens, small_screens, same_size?},
        state
      ) do
    endpoints =
      update_in(
        state.endpoints,
        [endpoint_name, :screen_sizes],
        &%{&1 | same_size?: same_size?, big_screens: big_screens, small_screens: small_screens}
      )

    {:noreply, %{state | endpoints: endpoints}}
  end
end
