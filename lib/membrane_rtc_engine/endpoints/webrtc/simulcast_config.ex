defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastConfig do
  @moduledoc """
  Module representing simulcast configuration for WebRTC endpoint.
  """

  alias Membrane.RTC.Engine.Track

  @typedoc """
  * `enabled` - whether to accept simulcast tracks or not.
  Setting this to false will result in rejecting all incoming
  simulcast tracks i.e. client will not send them.
  * `default_encoding` - function used to determine initial encoding
  this endpoint is willing to receive for given track.
  It is called for each track this endpoint subscribes for.
  """
  @type t() :: %__MODULE__{
          enabled: boolean(),
          default_encoding: (Track.t() -> String.t())
        }
  defstruct [
    :default_encoding,
    enabled: false
  ]
end
