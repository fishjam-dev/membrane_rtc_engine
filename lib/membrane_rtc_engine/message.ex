defmodule Membrane.RTC.Engine.Message do
  @moduledoc """
  Module describing messages RTC Engine can emit.

  Each Message contains RTC Engine PID under `rtc_engine` field.
  Thanks to it you can distinguish between the same messages but from different RTC Engine instances.
  """

  @type t() :: __MODULE__.EndpointCrashed.t() | __MODULE__.EndpointMessage.t()

  defmodule EndpointMessage do
    @moduledoc """
    Message emitted when an endpoint wishes to message the business logic.
    """

    @typedoc """
    Describes EndpointMessage message structure

    * `endpoint_id` - id of an endpoint
    * `message` - payload of the message sent by the endpoint
    """
    @type t() :: %__MODULE__{
            endpoint_id: any(),
            message: term()
          }

    @enforce_keys [:endpoint_id, :message]
    defstruct @enforce_keys
  end

  defmodule EndpointCrashed do
    @moduledoc """
    Message emitted when an endpoint crashes.
    """

    @typedoc """
    Describes EndpointCrashed Message structure.

    * `endpoint_id` - id of an endpoint that crashed
    """
    @type t() :: %__MODULE__{
            endpoint_id: any()
          }

    @enforce_keys [:endpoint_id]
    defstruct @enforce_keys
  end

  defmodule TrackPublished do
    @moduledoc """
    Message emitted when an endpoint publishes a track.
    """

    @typedoc """
    Describes NewTrack Message structure.

    * `endpoint_id` - id of an endpoint that published a track
    * `track` - track that was published
    """
    @type t() :: %__MODULE__{
            endpoint_id: any(),
            track: Membrane.RTC.Engine.Track.t()
          }

    @enforce_keys [:endpoint_id, :track]
    defstruct @enforce_keys
  end

  defmodule TrackUnpublished do
    @moduledoc """
    Message emitted when an endpoint unpublishes a track.
    """

    @typedoc """
    Describes TrackUnpublished Message structure.

    * `endpoint_id` - id of an endpoint that unpublishes a track
    * `track` - track that was unpublished
    """
    @type t() :: %__MODULE__{
            endpoint_id: any(),
            track: Membrane.RTC.Engine.Track.t()
          }

    @enforce_keys [:endpoint_id, :track]
    defstruct @enforce_keys
  end
end
