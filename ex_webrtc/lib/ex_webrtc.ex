defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

  alias __MODULE__.PeerConnectionHandler

  def_options rtc_engine: [
    spec: pid(),
    description: "Pid of parent Engine"
  ]

  def_input_pad :input,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :output,
    accepted_format: _any,
    availability: :on_request

  @impl true
  def handle_init(_ctx, opts) do
    state = %{rtc_engine: opts.rtc_engine}
    spec = [child(:handler, PeerConnectionHandler)]

    {[spec: spec, notify_parent: :ready], state}
  end

  @impl true
  def handle_parent_notification({:ready, _endpoints}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, _endpoint}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, _endpoint_id}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_metadata_updated, _track}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_variant_enabled, _track, _encoding}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_variant_disabled, _track, _encoding}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_metadata_updated, _endpoint}, _ctx, state) do
    # do nothing
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, _ctx, state) do
    dbg(tracks)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, tracks}, _ctx, state) do
    dbg(tracks)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:media_event, event}, _ctx, state) do
    dbg(event)
    {[], state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    dbg(msg)
    {[], state}
  end
end
