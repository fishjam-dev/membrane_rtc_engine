defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

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
  def handle_parent_notification({:ready, _endpoints}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    # other_endpoints =
    #   endpoints
    #   |> Enum.map(
    #     &%{
    #       id: &1.id,
    #       type: to_type_string(&1.type),
    #       metadata: &1.metadata,
    #       # Deprecated Field, use tracks field instead. It will be removed in the future.
    #       trackIdToMetadata: Endpoint.get_active_track_metadata(&1),
    #       tracks: &1 |> Endpoint.get_active_tracks() |> to_tracks_info()
    #     }
    #   )

    event =
      %{type: "connected", data: %{id: endpoint_id, otherEndpoints: []}}
      |> Jason.encode!()

    {[notify_parent: {:forward_to_parent, {:media_event, event}}], state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, _endpoint}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, _endpoint_id}, _ctx, state) do
    {[], state}
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
  def handle_parent_notification({:track_variant_disabled, track, encoding}, _ctx, state) do
    dbg({:track_variant_disabled, track})
    rid = to_rid(encoding)

    actions = forward("trackEncodingDisabled", %{endpointId: track.origin, trackId: track.id, encoding: rid})
    {actions, state}
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

    {[], %{state | outbound_tracks: outbound_tracks}}
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
         %{"type" => "disableTrackEncoding", "data" => %{"trackId" => track_id, "encoding" => rid}},
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
    {[notify_child: {:handler, {:offer, offer}}], state}
  end

  defp handle_custom(%{"type" => "candidate", "data" => candidate}, _ctx, state) do
    dbg(candidate)
    {[notify_child: {:handler, {:candidate, candidate}}], state}
  end

  defp handle_custom(%{"type" => "renegotiateTracks"}, _ctx, state) do
    tracks_types =
      state.outbound_tracks
      |> Map.values()
      |> Enum.filter(&(&1.status != :pending))
      |> Enum.map(& &1.type)

    media_count = %{
      audio: Enum.count(tracks_types, &(&1 == :audio)),
      video: Enum.count(tracks_types, &(&1 == :video))
    }

    media_event =
      %{
        type: "custom",
        data: %{type: "offerData", data: %{tracksTypes: media_count, integratedTurnServers: []}}
      }
      |> Jason.encode!()

    {[notify_parent: {:forward_to_parent, {:media_event, media_event}}], state}
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
  def handle_child_notification({:answer, answer}, :handler, _ctx, state) do
    dbg(answer)
    %{"sdp" => sdp} = answer

    mid_to_track_id =
    # state.inbound_tracks
    # |> Map.merge(state.outbound_tracks)
    # |> Map.values()
    # |> Map.new(&{&1.mid, &1.id})
    %{}


    actions = forward("custom", %{type: :sdpAnswer, data: %{
        type: "answer",
        sdp: sdp,
        midToTrackId: mid_to_track_id
      }})
    {actions, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :handler, _ctx, state) do
    dbg(candidate)

    actions = forward("custom", %{type: "candidate", data: candidate})
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

  defp forward(type, data) do
    event = Jason.encode!(%{"type" => type, "data" => data})
    msg = {:forward_to_parent, {:media_event, event}}
    [notify_parent: msg]
  end

  defp to_track_variant(rid) when rid in ["h", nil], do: :high
  defp to_track_variant("m"), do: :medium
  defp to_track_variant("l"), do: :low

  defp to_rid(:high), do: "h"
  defp to_rid(:medium), do: "m"
  defp to_rid(:low), do: "l"
end
