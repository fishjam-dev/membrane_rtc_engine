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
  """

  @type t() :: %__MODULE__{
          caps: RawVideo.t(),
          real_time: boolean()
        }
  defstruct caps: %Membrane.RawVideo{
              width: 400,
              height: 800,
              pixel_format: :I420,
              framerate: {24, 1},
              aligned: true
            },
            real_time: false
end
