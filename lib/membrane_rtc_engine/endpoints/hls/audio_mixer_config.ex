defmodule Membrane.RTC.Engine.Endpoint.HLS.AudioMixerConfig do
  @moduledoc """
  Module representing audio mixer configuration for the HLS endpoint.
  """

  @typedoc """
  * `output_framerate` - framerate of the output video, `{24, 1}` by default.
  * `output_width` - resolution width of the output video, `1280` by default.
  * `output_height` - resolution height of the output video, `720` by default.
  * `ffmpeg_filter` - defines the filter building function that positions input video,
                       Defaults to `default_ffmpeg_filter`
  """

  alias Membrane.RawAudio

  @type t() :: %__MODULE__{
          channels: RawAudio.channels_t(),
          sample_rate: RawAudio.sample_rate_t(),
          sample_format: SampleFormat.t()
        }
  defstruct channels: 1,
            sample_rate: 48_000,
            sample_format: :s16le
end
