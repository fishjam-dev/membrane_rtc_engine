defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC do
  @moduledoc """
  An Endpoint responsible for communicatiing with WebRTC client.
  """
  use Membrane.Bin

  require Logger
  require Membrane.Logger

  alias __MODULE__.MediaEvent
  alias __MODULE__.PeerConnectionHandler

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.ExWebRTC.{TrackReceiver, TrackSender}
  alias Membrane.RTC.Engine.Notifications.TrackNotification

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              ice_port_range: [
                spec: Enumerable.t(non_neg_integer()),
                description: "Range of ports that ICE will use for gathering host candidates.",
                default: nil
              ],
              metadata: [
                spec: any(),
                default: nil,
                description: "Endpoint metadata"
              ]

  def_input_pad :input,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :output,
    accepted_format: _any,
    availability: :on_request

  defmodule Track do
    @moduledoc false

    @enforce_keys [:status, :engine_track]
    defstruct @enforce_keys ++ [subscribe_ref: nil]

    @typedoc """
    Describes outbound tracks status
    :pending - the track is awaiting previous negotiation to finish
    :negotiating - track during negotiation
    :subscribing - waiting for subscription from engine
    :subscribed - completed subscription from engine
    """
    @type status :: :pending | :negotiating | :subscribing | :subscribed

    @type t :: %{
            status: status(),
            engine_track: Engine.Track.t(),
            subscribe_ref: reference()
          }
  end

  @impl true
  def handle_init(ctx, opts) do
    {_, endpoint_id} = ctx.name
    Logger.metadata(endpoint_id: endpoint_id)

    state =
      opts
      |> Map.from_struct()
      |> Map.merge(%{
        outbound_tracks: %{},
        inbound_tracks: %{},
        negotiation?: false
      })

    spec = [
      child(:connection_handler, %PeerConnectionHandler{
        endpoint_id: endpoint_id,
        ice_port_range: state.ice_port_range
      })
    ]

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, {track_id, rid}) = pad, _ctx, state) do
    if rid != :high, do: raise("ExWebrtcEndpoint handles only the high variant of tracks.")

    track = Map.fetch!(state.inbound_tracks, track_id)
    track_sender = %TrackSender{track: track, variant_bitrates: %{high: 1_500_000}}

    spec = [
      get_child(:connection_handler)
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

    spec =
      bin_input(pad)
      |> child({:track_receiver, track_id}, %TrackReceiver{
        track: track.engine_track,
        initial_target_variant: :h,
        keyframe_request_interval: Membrane.Time.seconds(5)
      })
      |> via_in(pad)
      |> get_child(:connection_handler)

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

    Membrane.Logger.info("endpoint ready, endpoints: #{inspect(endpoints)}")

    action = endpoint_id |> MediaEvent.connected(endpoints) |> MediaEvent.to_action()
    {action, state}
  end

  @impl true
  def handle_parent_notification({:new_endpoint, endpoint}, _ctx, state) do
    action = endpoint |> MediaEvent.endpoint_added() |> MediaEvent.to_action()
    Membrane.Logger.debug("endpoint added: #{inspect(endpoint)}")
    {action, state}
  end

  @impl true
  def handle_parent_notification({:endpoint_removed, endpoint_id}, _ctx, state) do
    action = endpoint_id |> MediaEvent.endpoint_removed() |> MediaEvent.to_action()
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
    {[], state}
  end

  @impl true
  def handle_parent_notification({:endpoint_metadata_updated, _endpoint}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, new_tracks}, _ctx, %{negotiation?: true} = state) do
    Membrane.Logger.debug("new parent queued tracks: #{log_tracks(new_tracks)}")

    outbound_tracks =
      new_tracks
      |> Map.new(&{&1.id, %Track{status: :pending, engine_track: &1}})
      |> Map.merge(state.outbound_tracks)

    {[], %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_parent_notification({:new_tracks, new_tracks}, _ctx, state) do
    Membrane.Logger.debug("new parent tracks: #{log_tracks(new_tracks)}")

    new_tracks = Map.new(new_tracks, &{&1.id, %Track{status: :pending, engine_track: &1}})

    new_tracks =
      state.outbound_tracks
      |> Map.filter(fn {_id, track} -> track.status == :pending end)
      |> Map.merge(new_tracks)
      |> Map.new(fn {id, track} -> {id, %{track | status: :negotiating}} end)

    state = update_in(state.outbound_tracks, &Map.merge(&1, new_tracks))

    tracks_added = get_new_tracks_actions(new_tracks)
    offer_data = get_offer_data(state.outbound_tracks)

    {tracks_added ++ offer_data, %{state | negotiation?: true}}
  end

  @impl true
  def handle_parent_notification({:remove_tracks, tracks}, _ctx, state) do
    track_ids = Enum.map(tracks, & &1.id)

    outbound_tracks = Map.drop(state.outbound_tracks, track_ids)

    Membrane.Logger.debug("remove tracks event for #{inspect(tracks)}")

    media_events =
      tracks
      |> Enum.group_by(& &1.origin)
      |> Enum.flat_map(fn {endpoint_id, tracks} ->
        track_ids = Enum.map(tracks, & &1.id)
        endpoint_id |> MediaEvent.tracks_removed(track_ids) |> MediaEvent.to_action()
      end)

    notify_handler = [notify_child: {:connection_handler, {:tracks_removed, track_ids}}]

    {media_events ++ notify_handler, %{state | outbound_tracks: outbound_tracks}}
  end

  @impl true
  def handle_parent_notification({:media_event, event}, ctx, state) do
    case deserialize(event) do
      {:ok, type, data} ->
        handle_media_event(type, data, ctx, state)

      {:error, :invalid_media_event} ->
        Membrane.Logger.warning("Invalid media event #{inspect(event)}. Ignoring.")
        {[], state}
    end

    %{"type" => type, "data" => data} = Jason.decode!(event)
    handle_media_event(type, data, ctx, state)
  end

  @impl true
  def handle_parent_notification(%TrackNotification{}, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(_msg, _ctx, state) do
    {[], state}
  end

  defp log_tracks(tracks) do
    tracks
    |> Enum.map(&Map.take(&1, [:type, :stream_id, :id, :origin, :encoding]))
    |> inspect()
  end

  defp handle_media_event(:connect, data, _ctx, state) do
    metadata = if is_map(data), do: Map.get(data, "metadata"), else: nil

    actions =
      if Map.has_key?(metadata, "displayName") do
        Logger.metadata(peer: metadata["displayName"])
        [notify_child: {:connection_handler, {:set_metadata, metadata["displayName"]}}]
      else
        []
      end

    {actions + [notify_parent: {:ready, metadata}], state}
  end

  defp handle_media_event(:disableTrackEncoding, %{trackId: track_id, encoding: rid}, _ctx, state) do
    encoding = to_track_variant(rid)
    {[notify_parent: {:disable_track_variant, track_id, encoding}], state}
  end

  defp handle_media_event(:sdp_offer, event, _ctx, state) do
    tracks =
      state.outbound_tracks
      |> Map.filter(fn {_id, t} -> t.status != :pending end)
      |> Map.new(fn {id, t} -> {id, t.engine_track} end)

    {[notify_child: {:connection_handler, {:offer, event, tracks}}], state}
  end

  defp handle_media_event(:candidate, candidate, _ctx, state) do
    {[notify_child: {:connection_handler, {:candidate, candidate}}], state}
  end

  defp handle_media_event(:renegotiatiate_tracks, _data, _ctx, %{negotiation?: true} = state) do
    {[], state}
  end

  defp handle_media_event(:renegotiatiate_tracks, _data, _ctx, state) do
    actions = get_offer_data(state.outbound_tracks)

    {actions, %{state | negotiation?: true}}
  end

  defp handle_media_event(type, event, _ctx, state) do
    Membrane.Logger.warning("unexpected media event: #{type}, #{event}")
    {[], state}
  end

  @impl true
  def handle_child_notification({:tracks, tracks}, :connection_handler, _ctx, state) do
    Membrane.Logger.debug("new webrtc tracks: #{log_tracks(tracks)}")

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
  def handle_child_notification({:tracks_removed, track_ids}, :connection_handler, ctx, state) do
    Membrane.Logger.debug("webrtc tracks removed")

    tracks = Map.take(state.inbound_tracks, track_ids)
    inbound_tracks = Map.drop(state.inbound_tracks, track_ids)

    track_senders =
      track_ids
      |> Enum.map(&{:track_sender, &1})
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    actions = [
      remove_children: track_senders,
      notify_parent: {:publish, {:removed_tracks, tracks}}
    ]

    {actions, %{state | inbound_tracks: inbound_tracks}}
  end

  @impl true
  def handle_child_notification(
        {:answer, answer, mid_to_track_id},
        :connection_handler,
        _ctx,
        state
      ) do
    actions = answer |> MediaEvent.sdp_answer(mid_to_track_id) |> MediaEvent.to_action()

    {actions, state}
  end

  @impl true
  def handle_child_notification({:candidate, candidate}, :connection_handler, _ctx, state) do
    actions = candidate |> MediaEvent.candidate() |> MediaEvent.to_action()
    {actions, state}
  end

  @impl true
  def handle_child_notification(
        :negotiation_done,
        :connection_handler,
        %{name: {_, endpoint_id}},
        %{negotiation?: true} = state
      ) do
    negotiated_tracks =
      state.outbound_tracks
      |> Map.filter(fn {_id, t} -> t.status == :negotiating end)
      |> Map.new(fn {id, track} ->
        ref = Engine.subscribe_async(state.rtc_engine, endpoint_id, id)
        {id, %{track | status: :subscribing, subscribe_ref: ref}}
      end)

    state = update_in(state.outbound_tracks, &Map.merge(&1, negotiated_tracks))
    pending_tracks = Map.filter(state.outbound_tracks, fn {_id, t} -> t.status == :pending end)

    if Enum.empty?(pending_tracks) do
      {[], %{state | negotiation?: false}}
    else
      tracks_added = get_new_tracks_actions(pending_tracks)

      new_tracks =
        Map.new(pending_tracks, fn {id, track} -> {id, %{track | status: :negotiating}} end)

      state = update_in(state.outbound_tracks, &Map.merge(&1, new_tracks))

      offer_data = get_offer_data(state.outbound_tracks)
      {tracks_added ++ offer_data, %{state | negotiation?: true}}
    end
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
  def handle_child_notification(_msg, _child, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_info({:subscribe_result, subscribe_ref, result}, _ctx, state) do
    {track_id, track} =
      Enum.find(state.outbound_tracks, fn {_id, t} -> t.subscribe_ref == subscribe_ref end)

    track = %{track | status: :subscribed, subscribe_ref: nil}

    outbound_tracks =
      case result do
        :ok -> Map.put(state.outbound_tracks, track_id, track)
        # TODO: build remove actions and notify handler
        :ignored -> Map.delete(state.outbound_tracks, track_id)
      end

    {[], %{state | outbound_tracks: outbound_tracks}}
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
      |> Enum.map(& &1.engine_track.type)

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

  defp get_new_tracks_actions(new_tracks) do
    new_tracks
    |> Map.values()
    |> Enum.map(& &1.engine_track)
    |> Enum.group_by(& &1.origin)
    |> Enum.flat_map(fn {origin, tracks} ->
      MediaEvent.tracks_added(origin, tracks)
      |> MediaEvent.to_action()
    end)
  end

  defp bitrate_notification(estimation) do
    {:bitrate_estimation, estimation}
  end

  defp deserialize(event) when is_binary(event) do
    case MediaEvent.decode(event) do
      {:ok, %{type: :custom, data: %{type: type, data: data}}} -> {:ok, type, data}
      {:ok, %{type: type, data: data}} -> {:ok, type, data}
      {:error, _reason} = error -> error
    end
  end
end
