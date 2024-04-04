defmodule Membrane.RTC.Engine.Subscriber do
  @moduledoc """
  Module representing a state needed for subscribing on tracks in RTC Engine.
  Besides a struct this module provide a helper functions that simplify whole process of subscribing on tracks
  and provides two different strategies for subscribing on tracks:
  * `:auto` - in this strategy subscriber subscribes on every provided track
  * `:manual` - in this strategy subscriber subscribe only on tracks from selected endpoints.
  After adding an endpoint subscriber will also subscribe on all existing tracks from this added endpoint.
  """

  use Bunch.Access

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Endpoint, Track}
  alias Membrane.RTC.Engine.Subscriber.{Automatic, Manual}

  @type tracks_t :: %{Track.id() => Track.t()}

  @type subscribe_result :: {[Track.t()], t()}

  @typedoc """
  This struct contains:
  * `endpoint_id` - id of endpoint
  * `rtc_engine` - pid of engine pipeline
  * `subscribe_mode` - mode of subscription. In `:auto` mode endpoint will
  try to subscribe on all available tracks. In `:manual` mode endpoint will
  subscribe only on tracks from endpoints that were previously added to state.
  * `tracks` - map of tracks on which that endpoint subscribed
  * `endpoints` - set of endpoints, endpoint will try to subscribe on tracks of these endpoints.
  Used only in `:manual` mode.
  """
  @type t() :: %__MODULE__{
          endpoint_id: Endpoint.id(),
          rtc_engine: pid(),
          subscribe_mode: :auto | :manual,
          tracks: tracks_t(),
          endpoints: MapSet.t()
        }

  @enforce_keys [:subscribe_mode, :rtc_engine, :endpoint_id]
  defstruct @enforce_keys ++ [tracks: %{}, endpoints: MapSet.new()]

  @doc """
  Callback invoked when a new tracks are added by engine.
  It should return an updated struct and it will try to subscribe on valid tracks.
  """
  @callback handle_new_tracks(tracks :: [Track.t()], subscriptions_state :: t()) :: t()

  @doc """
  Callback invoked when endpoint decided that it want to subscribe on specific endpoints.
  It should return an updated struct and it will try to subscribe on all tracks from these endpoints.
  This function is useless in `:auto` subscribe_mode.
  """
  @callback add_endpoints(endpoints :: [Endpoint.id()], subscriptions_state :: t()) :: t()

  @doc """
  Try to subscribe on tracks.

  It returns a list of valid tracks that were successfully subscribed
  """
  @spec subscribe_for_tracks([Track.t()], Endpoint.id(), pid()) :: [Track.t()]
  def subscribe_for_tracks(tracks, endpoint_id, rtc_engine) do
    {valid_tracks, invalid_tracks} =
      Enum.split_with(tracks, fn track ->
        case Engine.subscribe(rtc_engine, endpoint_id, track.id) do
          :ok ->
            true

          :ignored ->
            false
        end
      end)

    unless Enum.empty?(invalid_tracks) do
      msg = Enum.map_join(invalid_tracks, " ", & &1.id)
      Membrane.Logger.debug("Invalid tracks are: #{msg}")
    end

    valid_tracks
  end

  @spec update_tracks(t(), [Track.t()]) :: t()
  def update_tracks(state, tracks) do
    tracks = Map.new(tracks, &{&1.id, &1})
    Map.update!(state, :tracks, &Map.merge(&1, tracks))
  end

  @spec get_tracks(state :: t()) :: tracks_t()
  def get_tracks(state) do
    state.tracks
  end

  @spec get_track(state :: t(), track_id :: Track.id()) :: Track.t() | nil
  def get_track(state, track_id) do
    Map.get(state.tracks, track_id)
  end

  @spec remove_track(state :: t(), track_id :: Track.id()) :: t()
  def remove_track(state, track_id) do
    {_track, state} = pop_in(state, [:tracks, track_id])

    state
  end

  @spec update_endpoints(t(), [Endpoint.id()]) :: t()
  def update_endpoints(state, endpoints) do
    endpoints = MapSet.new(endpoints)
    Map.update!(state, :endpoints, &MapSet.union(&1, endpoints))
  end

  @spec pop_track!(t(), Track.id()) :: {Track.t(), t()}
  def pop_track!(state, track_id) do
    {removed_track, tracks} = Map.pop!(state.tracks, track_id)

    {removed_track, %{state | tracks: tracks}}
  end

  @doc """
  Provide new tracks for subscriber, that will try to subscribe on them if they satisfy
  conditions required by `:subscriber_mode`.

  The easiest usage would be to call this function during handling `:new_tracks` message from engine
  like in example below.

  ## Examples
      @impl true
      def handle_parent_notification({:new_tracks, tracks}, _ctx, state) do
        subscriber = Subscriber.handle_new_tracks(tracks, state.subscriber)

        {[], %{state | subscriber: subscriber}}
      end
  """
  @spec handle_new_tracks(tracks :: [Track.t()], subscriptions_state :: t()) :: t()
  def handle_new_tracks(tracks, %{subscribe_mode: :auto} = subscriptions_state) do
    Automatic.handle_new_tracks(tracks, subscriptions_state)
  end

  def handle_new_tracks(tracks, %{subscribe_mode: :manual} = subscriptions_state) do
    Manual.handle_new_tracks(tracks, subscriptions_state)
  end

  @doc """
  Provide new endpoint for subscriber, that will try to subscribe on their tracks.
  It makes sense only to use this method when subscriber is in mode `:manual`.
  """
  @spec add_endpoints(endpoints :: [Endpoint.id()], subscriptions_state :: t()) :: t()
  def add_endpoints(endpoints, %{subscribe_mode: :auto} = subscriptions_state) do
    Automatic.add_endpoints(endpoints, subscriptions_state)
  end

  def add_endpoints(endpoints, %{subscribe_mode: :manual} = subscriptions_state) do
    Manual.add_endpoints(endpoints, subscriptions_state)
  end
end
