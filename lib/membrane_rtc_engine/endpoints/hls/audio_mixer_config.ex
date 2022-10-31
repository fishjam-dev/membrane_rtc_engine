defmodule Membrane.RTC.Engine.Endpoint.HLS.AudioMixerConfig do
  @moduledoc """
  Module representing audio mixer configuration for the HLS endpoint.
  """

  @typedoc """
  * `channels` - number of channels inside a frame.
  * `sample_rate` - sample rate of the audio.
  * `sample_format` - format of samples.
  """

  alias Membrane.RawAudio

  @type t() :: %__MODULE__{
          channels: RawAudio.channels_t(),
          sample_rate: RawAudio.sample_rate_t(),
          sample_format: RawAudio.SampleFormat.t()
        }
  defstruct channels: 1,
            sample_rate: 48_000,
            sample_format: :s16le
end
