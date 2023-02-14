defmodule Membrane.RTC.Engine.Support.TestLayoutMaker do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker

  @impl true
  def init(_output_format), do: %{}

  @impl true
  def track_added(_state, track, _stream_format),
    do: {[], %{last_callback_invoked: {:track_added, track.id}}}

  @impl true
  def track_updated(_state, track, _stream_format),
    do: {[], %{last_callback_invoked: {:track_updated, track.id}}}

  @impl true
  def track_removed(_state, track), do: {[], %{last_callback_invoked: {:track_removed, track.id}}}
end
