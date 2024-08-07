defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

  alias __MODULE__.MediaEvent
  alias __MODULE__.PeerConnectionHandler

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.{TrackSender, TrackReceiver}
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
        inbound_tracks: %{},
        subscribed_tracks: MapSet.new()
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
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    track = Map.fetch!(state.outbound_tracks, track_id)
    # initial_target_variant = state.simulcast_config.initial_target_variant.(track)

    spec =
      bin_input(pad)
      |> child({:track_receiver, track_id}, %TrackReceiver{
        track: track,
        initial_target_variant: :h,
        keyframe_request_interval: Membrane.Time.seconds(5)
        # connection_allocator: state.connection_prober,
        # connection_allocator_module: state.connection_allocator_module,
        # telemetry_label: state.telemetry_label
      })
      |> via_in(pad)
      |> get_child(:handler)

    {[spec: spec], state}
  end

  @impl true
  def handle_parent_notification({:ready, _endpoints}, _ctx, state) do
    {[notify_child: {:handler, :add_peer_tracks}], state}
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

    new_outbound_tracks =
      Map.new(tracks, &{&1.id, &1})

    outbound_tracks =
      Map.merge(state.outbound_tracks, new_outbound_tracks)

    {[notify_child: {:handler, {:new_tracks, new_outbound_tracks}}],
     %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, tracks}, _ctx, state) do
    dbg(tracks)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:media_event, event}, ctx, state) do
    dbg(event, limit: :infinity, printable_limit: :infinity)

    %{"type" => type, "data" => data} = event

    handle_media_event(type, data, ctx, state)
  end

  @impl true
  def handle_parent_notification(
        %Membrane.RTC.Engine.Notifications.TrackNotification{},
        _ctx,
        state
      ) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(msg, _ctx, state) do
    dbg(msg)
    {[], state}
  end

  defp handle_media_event("custom", data, ctx, state) do
    handle_custom(data, ctx, state)
  end

  defp handle_media_event("connect", data, _ctx, state) do
    metadata = if is_map(data), do: Map.get(data, :metadata), else: nil
    {[notify_parent: {:ready, metadata}], state}
  end

  defp handle_media_event(
         "disableTrackEncoding",
         %{"trackId" => track_id, "encoding" => rid},
         _ctx,
         state
       ) do
    encoding = to_track_variant(rid)
    {[notify_parent: {:disable_track_variant, track_id, encoding}], state}
  end

  defp handle_media_event(type, event, _ctx, state) do
    dbg({:unexpected_event, type, event})
    {[], state}
  end

  defp handle_custom(%{"type" => "sdpAnswer", "data" => answer}, _ctx, state) do
    {[notify_child: {:handler, {:answer, answer}}], state}
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
  def handle_child_notification({:offer, offer}, :handler, _ctx, state) do
    dbg(offer)

    actions = MediaEvent.sdp_offer(offer) |> MediaEvent.to_action()

    {actions, state}
  end

  @impl true
  def handle_child_notification({:track, track}, :handler, _ctx, state) do
    dbg(track)

    track_ready = [{:notify_parent, {:track_ready, track.id, :high, track.encoding}}]

    inbound_tracks = Map.put(state.inbound_tracks, track.id, track)
    state = %{state | inbound_tracks: inbound_tracks}

    new_tracks = [notify_parent: {:publish, {:new_tracks, [track]}}]
    {new_tracks ++ track_ready, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :handler, _ctx, state) do
    dbg(candidate)

    actions = MediaEvent.candidate(candidate) |> MediaEvent.to_action()
    {actions, state}
  end

  @impl true
  def handle_child_notification(:negotiation_done, :handler, ctx, state) do
    new_outbound_tracks =
      Enum.filter(state.outbound_tracks, fn {track_id, _track} ->
        not MapSet.member?(state.subscribed_tracks, track_id)
      end)
      |> Enum.map(fn {track_id, _track} -> track_id end)

    {:endpoint, endpoint_id} = ctx.name

    {valid_tracks, invalid_tracks} =
      Enum.split_with(new_outbound_tracks, fn track_id ->
        case Engine.subscribe(state.rtc_engine, endpoint_id, track_id) do
          :ok ->
            true

          :ignored ->
            false
        end
      end)
      |> dbg()

    subscribed_tracks =
      state.subscribed_tracks
      |> then(&MapSet.union(&1, MapSet.new(valid_tracks)))
      |> then(&MapSet.difference(&1, MapSet.new(invalid_tracks)))

    # TODO: build_track_removed_actions

    dbg(subscribed_tracks)

    {[], %{state | subscribed_tracks: subscribed_tracks}}
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

  @impl true
  def handle_child_notification(msg, _child, _ctx, state) do
    dbg(msg)

    {[], state}
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
