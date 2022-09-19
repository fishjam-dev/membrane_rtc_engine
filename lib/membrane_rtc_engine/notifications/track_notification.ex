defmodule Membrane.RTC.Engine.Notifications.TrackNotification do
  @moduledoc """
  Struct describing a track notification sent to the instance of `Membrane.RTC.Engine` by Endpoints.

  TrackNotification command indicates that the engine should forward a notification about the specified track to all endpoints subscribed to that track.
  *Receivers can choose to ignore the notification*.

  The same struct will appear in `handle_other/3` callback on the receiver side.

  ## Publishing the notification
  Endpoint can publish a TrackNotification about any of the tracks it publishes, making the following pattern.
  ```elixir
  notification = %#{inspect(__MODULE__)}{
    track_id: "trackID_1",
    notification: :hello_world
  }

  {{:ok, notify: {:publish, notification}}, state}
  ```
  """

  alias Membrane.RTC.Engine.Track

  @enforce_keys [:track_id, :notification]
  defstruct @enforce_keys

  @typedoc """
  Type describing track notification struct

  - `track_id` - id of the track that this notification is about. This the id of the track that was assigned to the track when publishing it to the engine.
  - `notification` - payload of the notification. Can be anything
  """
  @type t() :: %__MODULE__{
          track_id: Track.id(),
          notification: term()
        }
end
