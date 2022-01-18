defmodule Membrane.RTC.Engine.TrackManager do
  @moduledoc """
  TBD
  """
  use GenServer
  alias Membrane.RTC.Engine.{SpeakersDetector, EndpointManager}

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
    {:ok, pid} = SpeakersDetector.start_link(ets_name: ets_name, track_manager: self())

    {:ok,
     %{
       endpoint_managers: %{},
       ets_name: ets_name,
       engine: engine_pid,
       speakers_detector: pid,
       calculating_priority?: false,
       vads: []
     }}
  end

  @impl true
  def handle_info({:vad_notification, endpoint_name, :speech}, state) do
    state = maybe_calculate_priority(state, [{:speech, endpoint_name} | state.vads])
    {:noreply, state}
  end

  @impl true
  def handle_info({:vad_notification, endpoint_name, :silence}, state) do
    {:noreply, %{state | vads: [{:silence, endpoint_name, System.monotonic_time()} | state.vads]}}
  end

  @impl true
  def handle_info({:vad_response, endpoint_to_tracks}, state) do
    send(state.engine, {:tracks_priority, endpoint_to_tracks})
    state = %{state | calculating_priority?: false}
    state = maybe_calculate_priority(state)
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
  def handle_info({:unregister_endpoint, endpoint_name}, state) do
    endpoints =
      Map.delete(
        state.endpoints,
        endpoint_name
      )

    state = %{state | endpoints: endpoints}
    state = maybe_calculate_priority(state, [{:unregister, endpoint_name} | state.vads])

    {:noreply, state}
  end

  @impl true
  def handle_info({:add_inbound_tracks, endpoint_name, tracks}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.add_tracks(&1, tracks, :inbound_tracks)
      )

    state = %{state | endpoints: endpoints}
    state = maybe_calculate_priority(state, [{:new_track, endpoint_name} | state.vads])

    {:noreply, state}
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
        &EndpointManager.prioritize_track(&1, track_id)
      )

    state = %{state | endpoints: endpoints}

    state = maybe_calculate_priority(state, [:prioritize_track | state.vads])

    {:noreply, state}
  end

  @impl true
  def handle_info({:unprioritize_track, endpoint_name, track_id}, state) do
    endpoints =
      Map.update!(
        state.endpoints,
        endpoint_name,
        &EndpointManager.unprioritize_track(&1, track_id)
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  @impl true
  def handle_info(
        {:prefered_video_sizes, endpoint_name, big_screens, medium_screens, small_screens,
         same_size?},
        state
      ) do
    endpoints =
      update_in(
        state.endpoints,
        [endpoint_name, :screen_sizes],
        &%{
          &1
          | same_size?: same_size?,
            big_screens: big_screens,
            medium_screens: medium_screens,
            small_screens: small_screens
        }
      )

    {:noreply, %{state | endpoints: endpoints}}
  end

  defp maybe_calculate_priority(state, vads \\ nil) do
    vads = vads || state.vads

    vads = Enum.reverse(vads)

    if not state.calculating_priority? and Enum.count(vads) > 0 do
      send(state.speakers_detector, {:calculcate_track_priorities, vads, state.endpoints})
      %{state | vads: [], calculating_priority?: true}
    else
      %{state | vads: vads}
    end
  end
end
