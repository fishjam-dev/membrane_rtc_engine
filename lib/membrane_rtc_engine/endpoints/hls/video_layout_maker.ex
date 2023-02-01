defmodule Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker do
  @moduledoc """
  behaviour defining how VideoLayout should change when new video track is added or when it's removed.
  """
  @type state() :: any()
  @type stream_format() :: Membrane.H264.t()
  @type output_format() :: RawVideo.t()
  @doc """
  output format represents parameters of output video created by compositor.
  """
  @type updated_layout() :: [{Track.id(), BaseVideoPlacement.t(), VideoTransformations.t()}]

  @callback init(output_format()) :: state()
  @callback track_added(state(), Track.t(), stream_format()) :: {updated_layout(), state()}
  @doc """
  track_updated is invoke when stream_format for a specific track has changed
  """
  @callback track_updated(state(), Track.t(), stream_format()) :: {updated_layout(), state()}
  @callback track_removed(state(), Track.t()) :: {updated_layout(), state()}
end
