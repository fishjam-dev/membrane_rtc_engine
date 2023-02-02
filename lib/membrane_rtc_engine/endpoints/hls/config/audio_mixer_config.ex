defmodule Membrane.RTC.Engine.Endpoint.HLS.AudioMixerConfig do
  @moduledoc """
  Module representing audio mixer configuration for the HLS endpoint.
  """

  @typedoc """
  * `channels` - number of channels inside a frame. Default value is 1.
  * `sample_rate` - sample rate of the audio. Default value is 48_000.
  * `sample_format` - format of samples. Default value is `:s16le`.
  """

  alias Membrane.RawAudio

  @type t() :: %__MODULE__{
          stream_format: RawAudio.t(),
          background: struct() | nil
        }
  defstruct stream_format: %RawAudio{
              channels: 1,
              sample_rate: 48_000,
              sample_format: :s16le
            },
            background: nil
end
