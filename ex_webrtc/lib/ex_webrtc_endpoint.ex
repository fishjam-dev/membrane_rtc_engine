defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc false
  use Membrane.Bin

  require Logger

  alias __MODULE__.MediaEvent
  alias __MODULE__.PeerConnectionHandler

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.{TrackSender, TrackReceiver}
  alias Membrane.RTC.Engine.Notifications.TrackNotification

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              ice_port_range: [
                spec: Enumerable.t(non_neg_integer()),
                description: "Range of ports that ICE will use for gathering host candidates.",
                default: nil
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
    endpoint_pid = self()

    subscriber_fun = fn ->
      subscriber(%{
        endpoint_id: endpoint_id,
        rtc_engine: opts.rtc_engine,
        endpoint_pid: endpoint_pid
      })
    end

    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{
        outbound_tracks: %{},
        inbound_tracks: %{},
        # Tracks for which the endpoint has subscribed
        subscribed_tracks: MapSet.new(),
        pending_tracks: MapSet.new(),
        negotiation?: false,
        # Outbound tracks, that have been added during negotation, and are queued for negotiation
        queued_new_tracks: [],
        subscriber: spawn(subscriber_fun)
      })

    spec = [
      child(:handler, %PeerConnectionHandler{
        endpoint_id: endpoint_id,
        ice_port_range: state.ice_port_range
      })
    ]

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
  def handle_pad_removed(Pad.ref(:input, track_id), _ctx, state) do
    {[remove_children: {:track_receiver, track_id}], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:output, {_track_id, _rid}), _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:ready, endpoints}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    Logger.info("endpoint ready, endpoints: #{inspect(endpoints)}")

    action = MediaEvent.connected(endpoint_id, endpoints) |> MediaEvent.to_action()
    {action, state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, endpoint}, _ctx, state) do
    action = MediaEvent.endpoint_added(endpoint) |> MediaEvent.to_action()
    Logger.info("endpoint added: #{inspect(endpoint.metadata)}")
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
  def handle_parent_notification({:track_variant_enabled, _track, _encoding}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:track_variant_disabled, _track, _encoding}, _ctx, state) do
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
  def handle_parent_notification({:new_tracks, new_tracks}, _ctx, %{negotiation?: true} = state) do
    Logger.debug("new parent queued tracks: #{log_tracks(new_tracks)}")
    queued_new_tracks = state.queued_new_tracks ++ new_tracks
    {[], %{state | queued_new_tracks: queued_new_tracks}}
  end

  @impl true
  def handle_parent_notification({:new_tracks, new_tracks}, _ctx, state) do
    Logger.debug("new parent tracks: #{log_tracks(new_tracks)}")

    new_tracks = state.queued_new_tracks ++ new_tracks

    {state, tracks_added} = new_tracks_added(state, new_tracks)
    offer_data = get_offer_data(state.outbound_tracks)

    {tracks_added ++ offer_data, %{state | queued_new_tracks: [], negotiation?: true}}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, tracks}, _ctx, state) do
    # TODO: remove tracks from queued track
    Logger.info("remove tracks event for #{inspect(tracks)}")
    {[], state}
  end

  @impl true
  def handle_parent_notification({:media_event, event}, ctx, state) do
    dbg(event, printable_limit: 1024)

    %{"type" => type, "data" => data} = Jason.decode!(event)

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

  defp log_tracks(tracks) do
    Enum.map(tracks, &Map.take(&1, [:type, :stream_id, :id, :origin, :encoding]))
    |> inspect()
  end

  defp handle_media_event("custom", data, ctx, state) do
    handle_custom(data, ctx, state)
  end

  defp handle_media_event("connect", data, _ctx, state) do
    metadata = if is_map(data), do: Map.get(data, "metadata"), else: nil

    display_name = String.to_atom(metadata["displayName"])

    Logger.metadata(peer: display_name)

    {[notify_parent: {:ready, metadata}, notify_child: {:handler, {:set_metadata, display_name}}],
     state}
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
    Logger.warning("unexpected media event: #{type}, #{event}")
    {[], state}
  end

  defp handle_custom(%{"type" => "sdpOffer", "data" => event}, _ctx, state) do
    IO.inspect({Map.values(state.outbound_tracks) |> log_tracks(), Map.drop(event, ["sdpOffer"])},
      label: "#{Logger.metadata()[:peer]} received sdp"
    )

    {[notify_child: {:handler, {:offer, event, state.outbound_tracks}}], state}
  end

  defp handle_custom(%{"type" => "candidate", "data" => candidate}, _ctx, state) do
    {[notify_child: {:handler, {:candidate, candidate}}], state}
  end

  defp handle_custom(%{"type" => "renegotiateTracks"}, _ctx, %{negotiation?: true} = state) do
    {[], state}
  end

  defp handle_custom(%{"type" => "renegotiateTracks"}, _ctx, state) do
    actions = get_offer_data(state.outbound_tracks)

    {actions, %{state | negotiation?: true}}
  end

  @impl true
  def handle_child_notification({:tracks, tracks}, :handler, _ctx, state) do
    Logger.debug("new child tracks: #{log_tracks(tracks)}")

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
    actions = MediaEvent.sdp_answer(answer, mid_to_track_id) |> MediaEvent.to_action()

    {actions, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :handler, _ctx, state) do
    actions = MediaEvent.candidate(candidate) |> MediaEvent.to_action()
    {actions, state}
  end

  @impl true
  def handle_child_notification(:negotiation_done, :handler, _ctx, %{negotiation?: true} = state) do
    new_outbound_tracks =
      Enum.filter(state.outbound_tracks, fn {track_id, _track} ->
        not MapSet.member?(state.subscribed_tracks, track_id) and
          not MapSet.member?(state.pending_tracks, track_id)
      end)
      |> Enum.map(fn {track_id, _track} -> track_id end)

    pending_tracks = MapSet.union(state.pending_tracks, MapSet.new(new_outbound_tracks))

    send(state.subscriber, {:subscribe, new_outbound_tracks})

    {state, actions} =
      if not Enum.empty?(state.queued_new_tracks) do
        {state, tracks_added} = new_tracks_added(state, state.queued_new_tracks)
        offer_data = get_offer_data(state.outbound_tracks)
        {%{state | negotiation?: true, queued_new_tracks: []}, tracks_added ++ offer_data}
      else
        {%{state | negotiation?: false}, []}
      end

    {actions, %{state | pending_tracks: pending_tracks}}
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

  @impl true
  def handle_info({:subscription_result, valid_tracks, invalid_tracks}, _ctx, state) do
    valid_tracks = MapSet.new(valid_tracks)
    invalid_tracks = MapSet.new(invalid_tracks)

    subscribed_tracks =
      state.subscribed_tracks
      |> MapSet.union(valid_tracks)
      |> MapSet.difference(invalid_tracks)

    pending_tracks =
      state.pending_tracks
      |> MapSet.difference(valid_tracks)
      |> MapSet.difference(invalid_tracks)

    IO.inspect(subscribed_tracks, label: "#{Logger.metadata()[:peer]} subscribed tracks")

    {[], %{state | subscribed_tracks: subscribed_tracks, pending_tracks: pending_tracks}}
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
      |> Enum.map(& &1.type)

    %{
      audio: Enum.count(tracks_types, &(&1 == :audio)),
      video: Enum.count(tracks_types, &(&1 == :video))
    }
  end

  defp get_offer_data(outbound_tracks) do
    outbound_tracks
    |> get_media_count()
    |> MediaEvent.offer_data([])
    |> MediaEvent.to_action()
  end

  defp new_tracks_added(state, new_tracks) do
    outbound_tracks =
      new_tracks
      |> Map.new(&{&1.id, &1})
      |> IO.inspect(label: "#{Logger.metadata()[:peer]} new outbound tracks")
      |> Map.merge(state.outbound_tracks)

    tracks_added =
      new_tracks
      |> Enum.group_by(& &1.origin)
      |> Enum.flat_map(fn {origin, tracks} ->
        MediaEvent.tracks_added(origin, tracks)
        |> MediaEvent.to_action()
      end)

    {%{state | outbound_tracks: outbound_tracks}, tracks_added}
  end

  defp subscriber(state) do
    receive do
      {:subscribe, new_outbound_tracks} ->
        {valid_tracks, invalid_tracks} =
          Enum.split_with(new_outbound_tracks, fn track_id ->
            Logger.debug("#{state.endpoint_id} subscribing for track #{track_id}")

            case Engine.subscribe(state.rtc_engine, state.endpoint_id, track_id) do
              :ok ->
                true

              :ignored ->
                false
            end
          end)

        send(state.endpoint_pid, {:subscription_result, valid_tracks, invalid_tracks})
    end

    subscriber(state)
  end
end
