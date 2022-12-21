defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorConfig do
  @moduledoc """
  Module representing compositor configuration for the HLS endpoint.

  Compositor is the element responsible for positioning input video streams
  into one video output.\"""

  @typedoc \"""
  * `caps` - output video specification in Membrane.RawVideo format
  """

  @type t() :: %__MODULE__{
          caps: RawVideo.t()
        }
  defstruct caps: %Membrane.RawVideo{
              width: 400,
              height: 800,
              pixel_format: :I420,
              framerate: {24, 1},
              aligned: true
            }
end
