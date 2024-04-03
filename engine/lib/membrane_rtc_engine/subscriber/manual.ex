defmodule Membrane.RTC.Engine.Subscriber.Manual do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Subscriber

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Subscriber

  @impl true
  def handle_new_tracks(tracks, subscriptions_state) do
    subscribed_tracks = Map.keys(subscriptions_state.tracks)

    new_subscribed_tracks =
      tracks
      |> Enum.filter(fn track ->
        MapSet.member?(subscriptions_state.endpoints, track.origin) and
          track.id not in subscribed_tracks
      end)
      |> Subscriber.subscribe_for_tracks(
        subscriptions_state.endpoint_id,
        subscriptions_state.rtc_engine
      )

    Subscriber.update_tracks(subscriptions_state, new_subscribed_tracks)
  end

  @impl true
  def add_endpoints(endpoints, subscriptions_state) do
    subscriptions_state = Subscriber.update_endpoints(subscriptions_state, endpoints)

    valid_tracks =
      subscriptions_state.rtc_engine
      |> Engine.get_tracks()
      |> Enum.filter(fn track ->
        MapSet.member?(subscriptions_state.endpoints, track.origin) and
          not is_map_key(subscriptions_state.tracks, track.id)
      end)

    origins = MapSet.new(valid_tracks, fn track -> track.origin end)

    not_found_endpoints =
      Enum.filter(endpoints, fn endpoint ->
        MapSet.member?(origins, endpoint)
      end)

    if not_found_endpoints != [] do
      Membrane.Logger.info(
        "Couldn't subscribe on any track from endpoints: #{not_found_endpoints}"
      )
    end

    new_subscribed_tracks =
      Subscriber.subscribe_for_tracks(
        valid_tracks,
        subscriptions_state.endpoint_id,
        subscriptions_state.rtc_engine
      )

    Subscriber.update_tracks(subscriptions_state, new_subscribed_tracks)
  end
end
