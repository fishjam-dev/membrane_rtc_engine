defmodule Membrane.RTC.Engine.Endpoint.Distributed do
  @moduledoc """
  An Endpoint responsible for communicatiing with WebRTC peer.

  It is responsible for sending and receiving media tracks from other WebRTC peer (e.g. web browser).
  """
  use Membrane.Endpoint
  import Membrane.RTC.Utils

  alias Membrane.RTC.Engine

  def_options pair_id: [
                spec: binary()
              ],
              type: [
                spec: :input | :output
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  def_output_pad :output,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  @impl true
  def handle_init(opts) do
    pair_id =
      case opts.pair_id do
        "ingress:" <> id -> id
        "egress:" <> id -> id
      end

    Registry.register(__MODULE__, pair_id, opts.type)

    {:ok,
     %{
       pair_id: pair_id,
       type: opts.type,
       tracks_from_twin: %{}
     }}
  end

  @impl true
  def handle_demand(pad, size, unit, _ctx, state) do
    send(state.twin, {:demand_from_twin, pad, size, unit})
    {:ok, state}
  end

  @impl true
  def handle_other({:demand_from_twin, pad, size, _unit}, ctx, state) do
    {actions, state} = maybe_do_action(:demand, reverse_pad(pad), size, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_prepared_to_playing(ctx, state) do
    case Registry.lookup(__MODULE__, state.pair_id) do
      [{pid, _val}] when pid == self() ->
        receive do
          {:twin_in_handle_prepared_to_playing, twin} ->
            {:ok, Map.put(state, :twin, twin)}
        after
          5_000 ->
            {{:error, :twin_distributed_endpoint_not_responding}, state}
        end

      [{pid1, val1}, {pid2, val2}] ->
        twin = if self() == pid1, do: pid2, else: pid1
        send(twin, {:twin_in_handle_prepared_to_playing, self()})
        {:ok, Map.put(state, :twin, twin)}
    end
  end

  @impl true
  def handle_other({:twin_in_handle_prepared_to_playing, twin}, _ctx, state),
    do: {:ok, state}

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    send(state.twin, {:track_from_twin_ready, track_id})
    {:ok, state}
  end

  @impl true
  def handle_other({:track_from_twin_ready, track_id}, _ctx, state) do
    track = state.tracks_from_twin[track_id]
    actions = [notify: {:track_ready, track_id, track.encoding, nil}]
    {{:ok, actions}, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:output, track_id) = pad, _ctx, state) do
    {actions, state} = flush_pad_queues(state, pad)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_write(Pad.ref(:input, _track_id) = pad, payload, _ctx, state) do
    # maybe add sending demands

    send(state.twin, {:buffer_from_twin, pad, payload})
    {:ok, state}
  end

  @impl true
  def handle_other({:buffer_from_twin, pad, payload}, ctx, state) do
    {actions, state} = maybe_do_action(:buffer, reverse_pad(pad), payload, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_other({:new_tracks, _tracks}, _ctx, %{type: :input} = state) do
    {:ok, state}
  end

  @impl true
  def handle_other({:new_tracks, tracks}, _ctx, %{type: :output} = state) do
    send(state.twin, {:new_tracks_from_twin, tracks})

    subscriptions = Enum.map(tracks, &{&1.id, :rtp})
    new_tracks = Map.new(tracks, &{&1.id, &1})

    {{:ok, notify: {:subscribe, subscriptions}}, state}
  end

  @impl true
  def handle_other({:new_tracks_from_twin, []}, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_other({:new_tracks_from_twin, tracks}, _ctx, state) when tracks != [] do
    new_tracks_map = Map.new(tracks, &{&1.id, &1})

    state =
      Map.update!(
        state,
        :tracks_from_twin,
        &Map.merge(&1, new_tracks_map)
      )

    {{:ok, notify: {:publish, {:new_tracks, tracks}}}, state}
  end

  @impl true
  def handle_other({:remove_tracks, tracks}, _ctx, state) do
    send(state.twin, {:remove_tracks_from_twin, tracks})
    {:ok, state}
  end

  @impl true
  def handle_other({:remove_tracks_from_twin, tracks}, _ctx, state) do
    inactive_tracks = Enum.map(tracks, &%{&1 | active?: false})

    tracks_ids = Enum.map(tracks, & &1.id)
    tracks_from_twin = Map.drop(state.tracks_from_twin, tracks_ids)
    state = %{state | tracks_from_twin: tracks_from_twin}

    {{:ok, notify: {:publish, {:removed_tracks, inactive_tracks}}}, state}
  end

  @impl true
  def handle_other({display_manager, _display_manager}, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_other({:caps_from_twin, pad, caps}, ctx, state) do
    {actions, state} = maybe_do_action(:caps, reverse_pad(pad), caps, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_other({:event_from_twin, pad, event}, ctx, state) do
    {actions, state} = maybe_do_action(:event, reverse_pad(pad), event, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_caps(pad, caps, ctx, state) do
    send(state.twin, {:caps_from_twin, pad, caps})
    {:ok, state}
  end

  @impl true
  def handle_event(pad, event, _ctx, state) do
    send(state.twin, {:event_from_twin, pad, event})
    {:ok, state}
  end

  @impl true
  def handle_shutdown(_reason, state) do
    Registry.unregister(__MODULE__, state.pair_id)
    :ok
  end

  defp reverse_pad(Pad.ref(:input, track_id)),
    do: Pad.ref(:output, track_id)

  defp reverse_pad(Pad.ref(:output, track_id)),
    do: Pad.ref(:input, track_id)

  defp maybe_do_action(action_type, pad, value, ctx, state) do
    if Map.has_key?(ctx.pads, pad) do
      actions = [{action_type, {pad, value}}]
      {actions, state}
    else
      pads_queues = Map.get(state, :pads_queues, %{})
      actions_queues = Map.get(pads_queues, pad, %{})
      action_queue = Map.get(actions_queues, action_type, []) ++ [value]

      actions_queues = Map.put(actions_queues, action_type, action_queue)
      pads_queues = Map.put(pads_queues, pad, actions_queues)
      state = Map.put(state, :pads_queues, pads_queues)

      {[], state}
    end
  end

  defp flush_pad_queues(state, pad) do
    actions =
      Enum.flat_map([:caps, :event, :buffer], fn action ->
        get_queued_actions(state, pad, action)
      end)

    pads_queues =
      Map.get(state, :pads_queues, %{})
      |> Map.put(pad, %{})

    state = Map.put(state, :pads_queues, pads_queues)

    {actions, state}
  end

  defp get_queued_actions(state, pad, action) do
    Map.get(state, :pads_queues, %{})
    |> Map.get(pad, %{})
    |> Map.get(action, [])
    |> Enum.map(fn val -> {action, {pad, val}} end)
  end
end
