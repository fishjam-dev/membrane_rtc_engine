defmodule Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker do
  @moduledoc """
  behaviour defining how VideoLayout should change when new track is added or when it's removed.
  """
  @type state() :: any()
  @type output_format() :: RawVideo.t()
  @doc """
  output format represents parameters of output video created by compositor.
  """
  @callback init(RawVideo.t()) :: state()
  @callback track_added(state, Track.t()) :: {[{Pad.ref_t(), VideoPlacement.t()}], state}
  @callback track_removed(state, Track.t()) :: {[{Pad.ref_t(), VideoPlacement.t()}], state}
end
