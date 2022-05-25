defmodule Membrane.RTC.Engine.Message do
  @moduledoc """
  Module describing messages RTC Engine can emit.

  Each Message contains RTC Engine PID under `rtc_engine` field.
  Thanks to it you can distinguish between the same messages but from different RTC Engine instances.
  """
  alias Membrane.RTC.Engine.Peer

  @type t() :: __MODULE__.MediaEvent.t() | __MODULE__.NewPeer.t() | __MODULE__.PeerLeft.t()

  defmodule MediaEvent do
    @moduledoc """
    Message emitted when RTC Engine needs to send some Media Event to the Client Library.
    """

    @typedoc """
    Describes MediaEvent Message structure.

    * `rtc_engine` - pid of RTC Engine instance which emitted this message
    * `to` - informs where this Media Event should be sent. If set to `:broadcast`, the Media Event
    should be sent to all peers. When set to `t:Membrane.RTC.Engine.Peer.id()`, the Media Event
    should be sent to that specified peer.
    * `data` - Media Event in serialized i.e. binary form
    """
    @type t() :: %__MODULE__{
            rtc_engine: pid(),
            to: Peer.id() | :broadcast,
            data: binary()
          }
    @enforce_keys [:rtc_engine, :to, :data]
    defstruct @enforce_keys
  end

  defmodule NewPeer do
    @moduledoc """
    Message emitted when a new peer from Client Library tries to join RTC Engine.

    You can reply to this message using: `Membrane.RTC.Engine.accept_peer/2` and
    `Membrane.RTC.Engine.deny_peer/2` or `Membrane.RTC.Engine.deny_peer/3`.
    """

    @typedoc """
    Describes NewPeer Message structure.

    * `rtc_engine` - pid of RTC Engine instance which emitted this message
    * `peer` - peer that tries to join to RTC Engine
    """
    @type t() :: %__MODULE__{
            rtc_engine: pid(),
            peer: Peer.t()
          }

    @enforce_keys [:rtc_engine, :peer]
    defstruct @enforce_keys
  end

  defmodule PeerLeft do
    @moduledoc """
    Message emitted when a peer left RTC Engine.
    """

    @typedoc """
    Describes PeerLeft Message structure.

    * `rtc_engine` - pid of RTC Engine instance which emitted this message
    * `peer` - peer that left RTC Engine
    """
    @type t() :: %__MODULE__{
            rtc_engine: pid(),
            peer: Peer.t()
          }

    @enforce_keys [:rtc_engine, :peer]
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
end
