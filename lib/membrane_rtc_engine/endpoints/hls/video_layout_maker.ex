defmodule Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker do
  @moduledoc """
  Behaviour defining how VideoLayout should change when new video track is added, updated or removed.
  """

  alias Membrane.RTC.Engine.Track
  alias Membrane.VideoCompositor.RustStructs.BaseVideoPlacement
  alias Membrane.VideoCompositor.VideoTransformations

  @type state() :: any()
  @type stream_format() :: Membrane.H264.t()

  @typedoc """
  Represent returned value from callbacks. This type is a list consisting of tuples.
  The first value of the tuple is a track id that helps to identify the tuple, then there is `BaseVideoPlacement`
  which is a `Membrane.VideoCompositor` struct that allows changing the position and resolution of a video.
  The last value is a `VideoTransformations` which is also a `Membrane.VideoCompositor` struct that allows to crop of video and rounding its corners. 
  """
  @type updated_layout() :: [{Track.id(), BaseVideoPlacement.t(), VideoTransformations.t()}]

  @typedoc """
  Represents parameters of output video created by compositor.
  """
  @type output_format() :: Membrane.RawVideo.t()

  @doc """
  Called when hls endpoint is intialized
  """
  @callback init(output_format()) :: state()

  @doc """
  Called when new track has been added to hls endpoint
  """
  @callback track_added(Track.t(), stream_format(), state()) :: {updated_layout(), state()}

  @doc """
  Called when `stream_format` for a specific track has changed
  """
  @callback track_updated(Track.t(), stream_format(), state()) :: {updated_layout(), state()}

  @doc """
  Called when a track has been removed
  """
  @callback track_removed(Track.t(), state()) :: {updated_layout(), state()}
end
