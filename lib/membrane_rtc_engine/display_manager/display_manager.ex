defmodule Membrane.RTC.Engine.DisplayManager do
  @moduledoc """
    Module responsible for limiting video tracks based on user preferences and VAD notifications
  """

  alias Membrane.RTC.Engine.Track
  alias Membrane.Time

  @enforce_keys [:video_tracks_limit, :endpoint_name]

  defstruct @enforce_keys ++
              [
                endpoint_tracks: %{},
                ends_of_speech: %{},
                screens_sizes: %{
                  same_size?: true,
                  big_screens: nil,
                  small_screens: nil,
                  medium_screens: nil
                },
                prioritized_tracks: MapSet.new()
              ]

  @type t :: %__MODULE__{
          video_tracks_limit: integer() | nil,
          endpoint_name: String.t(),
          prioritized_tracks: MapSet.new(),
          endpoint_tracks: %{String.t() => [Track.t()]},
          ends_of_speech: %{String.t() => {:silence | :speech, integer()} | nil},
          screens_sizes: %{
            same_size?: boolean(),
            big_screens: nil | integer(),
            medium_screens: nil | integer(),
            small_screens: nil | integer()
          }
        }

  @type ordered_tracks_and_state_t() :: {[Track.id()], t()}

  @type vad_notification_t() ::
          {:silence, String.t(), integer()}
          | {:speech, String.t(), integer()}
          | {:new_track, String.t()}
          | {:unregister, String.t()}

  @spec new(String.t(), integer() | nil) :: t()
  def new(endpoint_name, video_tracks_limit \\ nil) do
    %__MODULE__{endpoint_name: endpoint_name, video_tracks_limit: video_tracks_limit}
  end

  @spec get_monotonic_time() :: Time.t()
  def get_monotonic_time(), do: Time.monotonic_time()

  @spec add_vad_notification(t(), {:vad_notification, String.t(), :silence | :speech, integer()}) ::
          ordered_tracks_and_state_t()
  def add_vad_notification(state, {:vad_notification, endpoint_name, value, timestamp})
      when value in [:silence, :speech] do
    calculate_tracks_priority(state, [{value, endpoint_name, timestamp}])
  end

  @spec add_inbound_tracks(t(), [Track.t()]) :: t()
  def add_inbound_tracks(state, tracks) do
    endpoint_tracks =
      Map.update(
        state.endpoint_tracks,
        state.endpoint_name,
        tracks,
        &(&1 ++ tracks)
      )

    %{state | endpoint_tracks: endpoint_tracks}
  end

  @spec subscribe_tracks(t(), [Track.t()]) :: ordered_tracks_and_state_t()
  def subscribe_tracks(state, tracks) do
    endpoint_tracks =
      Enum.reduce(tracks, state.endpoint_tracks, fn track, acc ->
        Map.update(acc, track.origin, [track], &[track | &1])
      end)

    state = %{state | endpoint_tracks: endpoint_tracks}

    new_tracks = Enum.map(tracks, &{:new_track, &1.origin})

    calculate_tracks_priority(state, new_tracks)
  end

  @spec remove_tracks(t(), [Track.t()]) :: ordered_tracks_and_state_t()
  def remove_tracks(state, tracks) do
    endpoint_tracks =
      Enum.reduce(tracks, state.endpoint_tracks, fn track, acc ->
        Map.update!(acc, track.origin, fn tracks -> Enum.filter(tracks, &(&1 != track)) end)
      end)

    state = %{state | endpoint_tracks: endpoint_tracks}

    unregister_endpoints =
      state.endpoint_tracks
      |> Enum.reduce([], fn
        {endpoint_name, []}, acc -> [endpoint_name | acc]
        _endpoint_tracks, acc -> acc
      end)
      |> Enum.map(&{:unregister, &1})

    calculate_tracks_priority(state, unregister_endpoints)
  end

  @spec prioritize_track(t(), Track.id()) :: ordered_tracks_and_state_t()
  def prioritize_track(state, track_id) do
    state
    |> Map.update!(
      :prioritized_tracks,
      &MapSet.put(&1, track_id)
    )
    |> calculate_tracks_priority()
  end

  @spec unprioritize_track(t(), Track.id()) :: ordered_tracks_and_state_t()
  def unprioritize_track(state, track_id) do
    state
    |> Map.update!(
      :prioritized_tracks,
      &MapSet.delete(&1, track_id)
    )
    |> calculate_tracks_priority()
  end

  @spec change_prefered_video_sizes(t(), integer(), integer(), integer(), boolean()) :: t()
  def change_prefered_video_sizes(state, big_screens, medium_screens, small_screens, same_size?) do
    Map.update!(
      state,
      :screen_sizes,
      &%{
        &1
        | same_size?: same_size?,
          big_screens: big_screens,
          medium_screens: medium_screens,
          small_screens: small_screens
      }
    )
  end

  @spec calculate_tracks_priority(t(), [vad_notification_t()]) :: ordered_tracks_and_state_t()
  defp calculate_tracks_priority(state, vad_notification \\ []) do
    ends_of_speech =
      Enum.reduce(vad_notification, state.ends_of_speech, fn vad_notification, acc ->
        case vad_notification do
          {vad_value, endpoint_name, timestamp} ->
            Map.put(acc, endpoint_name, {vad_value, timestamp})

          {:new_track, endpoint_name} ->
            Map.put_new(acc, endpoint_name, nil)

          {:unregister, endpoint_name} ->
            Map.delete(acc, endpoint_name)
        end
      end)

    state = %{state | ends_of_speech: ends_of_speech}

    ordered_endpoint_names =
      ends_of_speech
      |> Enum.sort_by(fn {_endpoint_name, time} -> time end, &sort_vads/2)
      |> Enum.map(fn {endpoint_name, _time} -> endpoint_name end)

    ordered_tracks = Enum.flat_map(ordered_endpoint_names, &Map.get(state.endpoint_tracks, &1))

    inbound_tracks = Map.get(state.endpoint_tracks, state.endpoint_name)

    ordered_tracks =
      Enum.reject(ordered_tracks, fn track ->
        track.id in inbound_tracks or track.id in state.prioritized_tracks
      end)

    video_tracks_limit =
      if state.video_tracks_limit == nil do
        state.endpoint_tracks |> Map.values() |> Enum.flat_map(& &1) |> Enum.count()
      else
        max(state.video_tracks_limit - Enum.count(state.prioritized_tracks), 0)
      end

    ordered_tracks = ordered_tracks |> Enum.take(video_tracks_limit) |> Enum.map(& &1.id)

    tracks_priority = state.prioritized_tracks ++ ordered_tracks
    {tracks_priority, state}
  end

  defp sort_vads(_vad, nil), do: true
  defp sort_vads(nil, _vad), do: false
  defp sort_vads({:silence, timestamp1}, {:silence, timestamp2}), do: timestamp1 > timestamp2
  defp sort_vads({:silence, _timestamp1}, {:speech, _timestamp2}), do: false
  defp sort_vads({:speech, _timestamp1}, {:silence, _timestamp2}), do: true
  defp sort_vads({:speech, timestamp1}, {:speech, timestamp2}), do: timestamp1 < timestamp2
end
