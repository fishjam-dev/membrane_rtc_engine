defmodule Membrane.RTC.Engine.Subscription do
  @moduledoc false
  # Module representing subscription for track
  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.{Endpoint, Track}

  @typedoc """
  * `endpoint_id` - id of endpoint making subscription
  * `track_id` - id of track endpoint subscribes for
  * `status` - status of subscription. Subscription is
  `active` when track some endpoint subscribed for is linked
  to this endpoint and `pending` otherwise
  * `opts` - additional options
  """
  @type t() :: %__MODULE__{
          endpoint_id: Endpoint.id(),
          track_id: Track.id(),
          status: :pending | :active,
          opts: Engine.subscription_opts_t()
        }

  @enforce_keys [:endpoint_id, :track_id]
  defstruct @enforce_keys ++ [status: :pending, opts: []]
end
