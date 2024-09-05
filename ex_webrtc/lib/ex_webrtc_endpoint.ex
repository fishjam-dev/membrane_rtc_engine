defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

  alias __MODULE__.MediaEvent
  alias __MODULE__.PeerConnectionHandler

  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.TrackSender
  alias Membrane.RTC.Engine.Notifications.TrackNotification

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

  defmacrop bitrate_notification(estimation) do
    {:bitrate_estimation, estimation}
  end

  @impl true
  def handle_init(ctx, opts) do
    {_, endpoint_id} = ctx.name
    spec = [child(:handler, %PeerConnectionHandler{endpoint_id: endpoint_id})]

    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{
        outbound_tracks: %{},
        inbound_tracks: %{}
      })

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {track_id, rid}) = pad, _ctx, state) do
    if rid != :high, do: raise("temporary")

    track = Map.fetch!(state.inbound_tracks, track_id)
    track_sender = %TrackSender{track: track, variant_bitrates: %{high: 1_500_000}}

    spec = [
      get_child(:handler)
      |> via_out(pad)
      |> via_in(Pad.ref(:input, {track_id, rid}))
      |> child({:track_sender, track_id}, track_sender, get_if_exists: true)
      |> via_out(pad)
      |> bin_output(pad)
    ]

    {[spec: spec], state}
  end

  @impl true
  def handle_parent_notification({:ready, endpoints}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    actions = MediaEvent.connected(endpoint_id, endpoints) |> MediaEvent.to_action()
    {actions, state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, endpoint}, _ctx, state) do
    action = MediaEvent.endpoint_added(endpoint) |> MediaEvent.to_action()
    {action, state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, endpoint_id}, _ctx, state) do
    action = MediaEvent.endpoint_removed(endpoint_id) |> MediaEvent.to_action()
    {action, state}
  end

  @impl true
  def handle_parent_notification({:track_metadata_updated, _track}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_variant_enabled, track, _encoding}, _ctx, state) do
    dbg(track)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_variant_disabled, track, _encoding}, _ctx, state) do
    dbg({:track_variant_disabled, track})
    # rid = to_rid(encoding)

    # actions = forward("trackEncodingDisabled", %{endpointId: track.origin, trackId: track.id, encoding: rid})
    # actions = MediaEvent.track_variant_disabled()

    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_metadata_updated, _endpoint}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, _ctx, state) do
    dbg(tracks)

    outbound_tracks =
      Enum.reduce(tracks, state.outbound_tracks, fn track, acc ->
        Map.put(acc, track.id, track)
      end)

    tracks_added =
      tracks
      |> Enum.group_by(& &1.origin)
      |> Enum.flat_map(fn {origin, tracks} ->
        MediaEvent.tracks_added(origin, tracks)
        |> MediaEvent.to_action()
      end)

    offer_data =
      outbound_tracks
      |> get_media_count()
      |> MediaEvent.offer_data([])
      |> MediaEvent.to_action()

    {tracks_added ++ offer_data, %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, tracks}, _ctx, state) do
    dbg(tracks)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:media_event, event}, ctx, state) do
    dbg(event)

    event
    |> Jason.decode!()
    |> handle_media_event(ctx, state)
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    dbg(msg)
    {[], state}
  end

  defp handle_media_event(%{"type" => "custom", "data" => data}, ctx, state) do
    handle_custom(data, ctx, state)
  end

  defp handle_media_event(
         %{"type" => "connect", "data" => %{"metadata" => metadata}},
         _ctx,
         state
       ) do
    {[notify_parent: {:ready, metadata}], state}
  end

  defp handle_media_event(
         %{
           "type" => "disableTrackEncoding",
           "data" => %{"trackId" => track_id, "encoding" => rid}
         },
         _ctx,
         state
       ) do
    encoding = to_track_variant(rid)
    {[notify_parent: {:disable_track_variant, track_id, encoding}], state}
  end

  defp handle_media_event(event, _ctx, state) do
    dbg({:unexpected_event, event})
    {[], state}
  end

  defp handle_custom(%{"type" => "sdpOffer", "data" => %{"sdpOffer" => offer}}, _ctx, state) do
    {[notify_child: {:handler, {:offer, offer, state.outbound_tracks}}], state}
  end

  defp handle_custom(%{"type" => "candidate", "data" => candidate}, _ctx, state) do
    dbg(candidate)
    {[notify_child: {:handler, {:candidate, candidate}}], state}
  end

  defp handle_custom(%{"type" => "renegotiateTracks"}, _ctx, state) do
    actions =
      state.outbound_tracks
      |> get_media_count()
      |> MediaEvent.offer_data([])
      |> MediaEvent.to_action()

    {actions, state}
  end

  @impl true
  def handle_child_notification({:tracks, tracks}, :handler, _ctx, state) do
    dbg(tracks)

    tracks_ready =
      Enum.map(tracks, fn track ->
        {:notify_parent, {:track_ready, track.id, :high, track.encoding}}
      end)

    inbound_tracks = Map.new(tracks, fn track -> {track.id, track} end)
    state = %{state | inbound_tracks: inbound_tracks}

    new_tracks = [notify_parent: {:publish, {:new_tracks, tracks}}]
    {new_tracks ++ tracks_ready, state}
  end

  @impl true
  def handle_child_notification({:answer, answer, mid_to_track_id}, :handler, _ctx, state) do
    dbg(answer)

    actions = MediaEvent.sdp_answer(answer, mid_to_track_id) |> MediaEvent.to_action()

    {actions, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :handler, _ctx, state) do
    dbg(candidate)

    actions = MediaEvent.candidate(candidate) |> MediaEvent.to_action()
    {actions, state}
  end

  @impl true
  def handle_child_notification(
        {:estimation, estimations},
        {:track_sender, track_id},
        _ctx,
        state
      ) do
    notification = %TrackNotification{
      track_id: track_id,
      notification: bitrate_notification(estimations)
    }

    {[notify_parent: {:publish, notification}], state}
  end

  @spec to_rid(atom()) :: String.t()
  def to_rid(:high), do: "h"
  def to_rid(:medium), do: "m"
  def to_rid(:low), do: "l"

  defp to_track_variant(rid) when rid in ["h", nil], do: :high
  defp to_track_variant("m"), do: :medium
  defp to_track_variant("l"), do: :low

  defp get_media_count(tracks) do
    tracks_types =
      tracks
      |> Map.values()
      # |> Enum.filter(&(&1.status != :pending))
      |> Enum.map(& &1.type)

    %{
      audio: Enum.count(tracks_types, &(&1 == :audio)),
      video: Enum.count(tracks_types, &(&1 == :video))
    }
  end
end
