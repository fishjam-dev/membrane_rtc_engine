defmodule Membrane.RTC.Engine.Subscription do
  @moduledoc false
  # Module representing subscription for track
  alias Membrane.RTC.Engine.{Endpoint, Track}

  @typedoc """
  Subscription options.

  * `default_simulcast_encoding` - initial encoding that
  endpoint making subscription wants to receive
  """
  @type opts_t :: [default_simulcast_encoding: String.t()]

  @typedoc """
  * `endpoint_id` - id of endpoint making subscription for track
  * `track_id` - id of track endpoint subscribes for
  * `format` - format of track endpoint subscribes for
  * `status` - status of subscription. Subscription is `active` when
  given track is linked to given endpoint and `pending` otherwise
  * `opts` - additional options
  """
  @type t() :: %___MODULE__{
          endpoint_id: Endpoint.id(),
          track_id: Track.id(),
          format: Track.format(),
          status: :created | :pending | :active,
          opts: opts_t()
        }

  @enforce_keys [:endpoint_id, :track_id, :format, :status, :opts]
  defstruct @enforce_keys
end
