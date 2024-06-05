defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

  alias __MODULE__.PeerConnectionHandler

  def_options(
    rtc_engine: [
      spec: pid(),
      description: "Pid of parent Engine"
    ]
  )

  def_input_pad(:input,
    accepted_format: _any,
    availability: :on_request
  )

  def_output_pad(:output,
    accepted_format: _any,
    availability: :on_request
  )

  @impl true
  def handle_init(ctx, opts) do
    state = %{rtc_engine: opts.rtc_engine}

    {_, endpoint_id} = ctx.name
    spec = [child(:handler, %PeerConnectionHandler{endpoint_id: endpoint_id})]

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_added(_pad, _ctx, state) do
    {[], state}
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
    actions =
      event
      |> Jason.decode!()
      |> case do
        %{"type" => "offer", "data" => offer} -> {:offer, offer}
        %{"type" => "answer", "data" => answer} -> {:offer, answer}
        %{"type" => "candidate", "data" => cand} -> {:candidate, cand}
        _other -> nil
      end
      |> case do
        nil -> []
        other -> [notify_child: {:handler, other}]
      end

    {actions, state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    dbg(msg)
    {[], state}
  end

  @impl true
  def handle_child_notification({:tracks, tracks}, :handler, _ctx, state) do
    tracks_ready =
      Enum.map(tracks, fn track ->
        {:notify_parent, {:track_ready, track.id, :high, track.encoding}}
      end)

    new_tracks = [notify_parent: {:publish, {:new_tracks, tracks}}]
    {new_tracks ++ tracks_ready, state}
  end

  @impl true
  def handle_child_notification({:answer, answer}, :handler, _ctx, state) do
    actions = forward_media_event("answer", answer)
    {[notify_parent: :ready] ++ actions, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :handler, _ctx, state) do
    actions = forward_media_event("candidate", candidate)
    {actions, state}
  end

  defp forward_media_event(type, data) do
    event = Jason.encode!(%{"type" => type, "data" => data})
    msg = {:forward_to_parent, {:media_event, event}}
    [notify_parent: msg]
  end
end
