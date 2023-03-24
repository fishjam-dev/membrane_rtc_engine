defmodule Membrane.RTC.Engine.Support.TestLayoutMaker do
  @moduledoc false

  @behaviour Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker

  @impl true
  def init(_output_format, _options), do: %{}

  @impl true
  def track_added(track, _stream_format, _state),
    do: {[], %{last_callback_invoked: {:track_added, track.id}}}

  @impl true
  def track_updated(track, _stream_format, _state),
    do: {[], %{last_callback_invoked: {:track_updated, track.id}}}

  @impl true
  def track_removed(track, _state), do: {[], %{last_callback_invoked: {:track_removed, track.id}}}
end
