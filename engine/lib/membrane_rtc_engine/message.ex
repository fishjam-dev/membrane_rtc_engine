defmodule Membrane.RTC.Engine.Message do
  @moduledoc """
  Module describing messages RTC Engine can emit.

  Each Message contains the PID of the Engine under `rtc_engine` field.
  Thanks to it you can distinguish between the same messages but from different RTC Engine instances.
  """

  alias Membrane.RTC.Engine.{Endpoint, Track}

  @type t() ::
          __MODULE__.EndpointAdded.t()
          | __MODULE__.EndpointCrashed.t()
          | __MODULE__.EndpointMessage.t()
          | __MODULE__.EndpointRemoved.t()
          | __MODULE__.TrackAdded.t()
          | __MODULE__.TrackRemoved.t()

  defmodule EndpointAdded do
    @moduledoc """
    Message emitted when an endpoint gets added.
    """

    @typedoc """
    Describes EndpointAdded message structure.

    * `endpoint_id` - id of the endpoint that was added
    * `endpoint_type` - type of the endpoint that was added
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type()
          }

    @enforce_keys [:endpoint_id, :endpoint_type]
    defstruct @enforce_keys
  end

  defmodule EndpointCrashed do
    @moduledoc """
    Message emitted when an endpoint crashes.
    """

    @typedoc """
    Describes EndpointCrashed message structure.

    * `endpoint_id` - id of the endpoint that crashed
    * `endpoint_type` - type of the endpoint that crashed
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type()
          }

    @enforce_keys [:endpoint_id, :endpoint_type]
    defstruct @enforce_keys
  end

  defmodule EndpointMessage do
    @moduledoc """
    Message emitted when an endpoint wishes to message the business logic.
    """

    @typedoc """
    Describes EndpointMessage message structure

    * `endpoint_id` - id of the endpoint
    * `endpoint_type` - type of the endpoint
    * `message` - payload of the message sent by the endpoint
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type(),
            message: term()
          }

    @enforce_keys [:endpoint_id, :endpoint_type, :message]
    defstruct @enforce_keys
  end

  defmodule EndpointRemoved do
    @moduledoc """
    Message emitted when an endpoint gets removed.
    """

    @typedoc """
    Describes EndpointRemoved message structure.

    * `endpoint_id` - id of the endpoint that was removed
    * `endpoint_type` - type of the endpoint that was removed
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type()
          }

    @enforce_keys [:endpoint_id, :endpoint_type]
    defstruct @enforce_keys
  end

  defmodule TrackAdded do
    @moduledoc """
    Message emitted when a track gets added.
    """

    @typedoc """
    Describes TrackAdded message structure.

    * `endpoint_id` - id of the endpoint that published the track
    * `endpoint_type` - type of the endpoint that published the track
    * `track_id` - id of the track that was added
    * `track_type` - type of the track that was added
    * `track_encoding` - encoding of the track that was added
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type(),
            track_id: Track.id(),
            track_type: :audio | :video,
            track_encoding: Track.encoding()
          }

    @enforce_keys [:endpoint_id, :endpoint_type, :track_id, :track_type, :track_encoding]
    defstruct @enforce_keys
  end

  defmodule TrackRemoved do
    @moduledoc """
    Message emitted when a track gets removed.
    """

    @typedoc """
    Describes TrackRemoved message structure.

    * `endpoint_id` - id of the endpoint that published the track
    * `endpoint_type` - type of the endpoint that published the track
    * `track_id` - id of the track that was removed
    * `track_type` - type of the track that was added
    * `track_encoding` - encoding of the track that was added
    """
    @type t() :: %__MODULE__{
            endpoint_id: Endpoint.id(),
            endpoint_type: Endpoint.type(),
            track_id: Track.id(),
            track_type: :audio | :video,
            track_encoding: Track.encoding()
          }

    @enforce_keys [:endpoint_id, :endpoint_type, :track_id, :track_type, :track_encoding]
    defstruct @enforce_keys
  end
end
