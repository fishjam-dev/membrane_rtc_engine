if Code.ensure_loaded?(Membrane.RawVideo) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorConfig do
    @moduledoc """
    Module representing compositor configuration for the HLS endpoint.

    Compositor is the element responsible for positioning input video streams
    into one video output.
    """

    @typedoc """
    * `stream_format` - output video specification in Membrane.RawVideo format
    * `layout_module` - Module implementing `Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker`
      behavior that should be used by the HLS endpoint.
    """

    @type t() :: %__MODULE__{
            stream_format: Membrane.RawVideo.t(),
            layout_module: module(),
            background: struct() | nil
          }
    defstruct stream_format: %Membrane.RawVideo{
                width: 400,
                height: 800,
                pixel_format: :I420,
                framerate: {24, 1},
                aligned: true
              },
              layout_module: Membrane.RTC.Engine.Endpoint.HLS.MobileLayoutMaker,
              background: nil
  end
end
