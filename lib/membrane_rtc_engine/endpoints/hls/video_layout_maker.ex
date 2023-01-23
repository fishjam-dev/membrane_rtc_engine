defmodule Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker do
  @moduledoc """
  behaviour defining how VideoLayout should change when new track is added or when it's removed.
  """
  @type state() :: any()
  @type caps() :: Membrane.H264.t()
  @type track_id() :: String.t()
  @type output_format() :: RawVideo.t()
  @type updated_layout :: [{track_id, BaseVideoPlacement.t(), VideoTransformations.t()}]
  @doc """
  output format represents parameters of output video created by compositor.
  """
  @callback init(output_format()) :: state()
  @callback track_added(state, Track.t(), caps) :: {updated_layout, state}
  # track_updated is called when caps for specific track hqw changed
  @callback track_updated(state, Track.t(), caps) :: {updated_layout, state}
  @callback track_removed(state, Track.t()) :: {updated_layout, state}
end
