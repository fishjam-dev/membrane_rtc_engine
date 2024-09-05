defmodule Membrane.WebRTC.Track.Constraints do
  @moduledoc false

  alias ExSDP.Attribute.{FMTP, RTPMapping}
  alias Membrane.WebRTC.{EndpointBin, Extension}

  @type t :: %__MODULE__{
          codecs_filter: ({RTPMapping.t(), FMTP.t() | nil} -> boolean()),
          enabled_extensions: [Extension.t()],
          simulcast?: boolean(),
          endpoint_direction: EndpointBin.direction()
        }

  @enforce_keys [:codecs_filter, :enabled_extensions, :endpoint_direction, :simulcast?]
  defstruct @enforce_keys
end
