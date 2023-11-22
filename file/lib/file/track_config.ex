defmodule Membrane.RTC.Engine.Endpoint.File.TrackConfig do
  alias Membrane.RTC.Engine.Track
  alias ExSDP.Attribute.FMTP

  @typedoc """
  The options passed to the `Membrane.RTC.Engine.Track.new/7`
  """
  @type t() :: %__MODULE__{
          type: :audio | :video,
          encoding: Track.encoding(),
          clock_rate: Membrane.RTP.clock_rate_t(),
          fmtp: FMTP.t(),
          opts: Track.opts_t()
        }

  @enforce_keys [:type, :encoding, :clock_rate, :fmtp]

  defstruct @enforce_keys ++ [opts: []]
end
