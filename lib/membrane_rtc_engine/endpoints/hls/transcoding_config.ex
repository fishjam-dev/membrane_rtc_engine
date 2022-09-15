defmodule Membrane.RTC.Engine.Endpoint.HLS.TranscodingConfig do
  @moduledoc """
  Module representing transcoding configuration for the HLS endpoint.
  """

  @typedoc """
  * `enabled?` - when false, transcoding will not be bypassed.
  * `output_framerate` - framerate of the output video, `{24, 1}` by default.
  * `output_width` - resolution width of the output video, `1280` by default.
  * `output_height` - resolution height of the output video, `720` by default.
  """
  @type t() :: %__MODULE__{
          enabled?: boolean(),
          output_framerate: {integer(), integer()},
          output_width: integer(),
          output_height: integer()
  }
  defstruct enabled?: false,
            output_framerate: {24, 1},
            output_width: 1280,
            output_height: 720
end
