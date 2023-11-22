defmodule Membrane.RTC.Engine.Endpoint.File.TrackConfig do
  @moduledoc """
  Track Configuration, used to create track after
  initialization of File Endpoint.
  """
  alias ExSDP.Attribute.FMTP
  alias Membrane.RTC.Engine.Track

  @typedoc """
  The options passed to the `Membrane.RTC.Engine.Track.new/7`
  """
  @type t() :: %__MODULE__{
          type: :audio | :video,
          stream_id: String.t() | nil,
          encoding: Track.encoding(),
          clock_rate: Membrane.RTP.clock_rate_t(),
          fmtp: FMTP.t(),
          opts: Track.opts_t()
        }

  @enforce_keys [:type, :encoding, :clock_rate, :fmtp]

  defstruct @enforce_keys ++ [opts: [], stream_id: nil]
end
