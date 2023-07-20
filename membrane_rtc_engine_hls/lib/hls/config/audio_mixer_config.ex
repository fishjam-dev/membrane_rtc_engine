if Code.ensure_loaded?(Membrane.RawAudio) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.AudioMixerConfig do
    @moduledoc """
    Module representing audio mixer configuration for the HLS endpoint.
    """

    alias Membrane.RawAudio

    @typedoc """
    * `stream_format` - defines audio mixer output stream_format.
    """
    @type t() :: %__MODULE__{
            stream_format: RawAudio.t()
          }
    defstruct stream_format: %RawAudio{
                channels: 1,
                sample_rate: 48_000,
                sample_format: :s16le
              }
  end
end
