defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorConfig do
  @moduledoc """
  Module representing compositor configuration for the HLS endpoint.

  Compositor is the element responsible for positioning input video streams
  into one video output. It operates on raw frames and mixes given pads according
  to the provided `ffmpeg_filter` parameter.
  """

  @typedoc """
  * `output_framerate` - framerate of the output video, `{24, 1}` by default.
  * `output_width` - resolution width of the output video, `1280` by default.
  * `output_height` - resolution height of the output video, `720` by default.
  * `ffmpeg_filter` - defines the filter building function that positions input video,
                       Defaults to `default_ffmpeg_filter`
  * `output_directory` - name of the directory in which hls stream will be saved.
  """

  alias Membrane.RTC.Engine.Endpoint.HLS.FFmpegFilter

  @type t() :: %__MODULE__{
          output_framerate: {integer(), integer()},
          output_width: integer(),
          output_height: integer(),
          ffmpeg_filter: fun(),
          output_directory: String.t() | nil
        }
  defstruct output_framerate: {24, 1},
            output_width: 1280,
            output_height: 720,
            ffmpeg_filter: &FFmpegFilter.default_ffmpeg_filter/3,
            output_directory: nil
end
