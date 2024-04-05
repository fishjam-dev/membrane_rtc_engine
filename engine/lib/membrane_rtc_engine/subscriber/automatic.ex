defmodule Membrane.RTC.Engine.Subscriber.Automatic do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Subscriber

  require Membrane.Logger

  alias Membrane.RTC.Engine.Subscriber

  @impl true
  def handle_new_tracks(tracks, subscriptions_state) do
    new_subscribed_tracks =
      tracks
      |> Subscriber.subscribe_for_tracks(
        subscriptions_state.endpoint_id,
        subscriptions_state.rtc_engine
      )

    Subscriber.update_tracks(subscriptions_state, new_subscribed_tracks)
  end

  @impl true
  def add_endpoints(_endpoints, subscriptions_state) do
    Membrane.Logger.warning("""
    Unexepected usage of method.
    If you want to add tracks manually set `:subscribe_mode` option to `:manual`.
    """)

    subscriptions_state
  end
end
