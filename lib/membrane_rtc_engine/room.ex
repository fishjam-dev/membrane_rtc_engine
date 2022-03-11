defmodule Membrane.RTC.Engine.Room do
  @moduledoc false

  use GenServer

  use OpenTelemetryDecorator
  import Membrane.RTC.Utils

  alias Membrane.RTC.Engine.{
    Endpoint,
    MediaEvent,
    Message,
    Track,
    Peer,
    DisplayManager
  }

  alias Membrane.RTC.Engine

  require Membrane.Logger

  @registry_name __MODULE__.Registry.Dispatcher

  @type options_t() :: [
          id: binary(),
          ingress_node: Node.t(),
          egress_node: Node.t(),
          ingress_options: Engine.options_t(),
          egress_options: Engine.options_t(),
          ingress_process_options: GenServer.options(),
          egress_process_options: GenServer.options()
        ]

  @spec start(options :: options_t(), process_options :: GenServer.options()) ::
          GenServer.on_start()
  def start(options, process_options) do
    GenServer.start(__MODULE__, options, process_options)
  end

  @doc """
  Registers process with pid `who` for receiving messages from RTC Engine
  """
  @spec register(room :: pid(), who :: pid()) :: :ok
  def register(room, who \\ self()) do
    send(room, {:register, who})
    :ok
  end

  @doc """
  Unregisters process with pid `who` from receiving messages from RTC Engine
  """
  @spec unregister(room :: pid(), who :: pid()) :: :ok
  def unregister(room, who \\ self()) do
    send(room, {:unregister, who})
    :ok
  end

  @impl true
  def init(options) do
    id = options[:id]
    egress_node = options[:egress_node]
    ingress_node = options[:ingress_node]

    {:ok, ingress_engine} =
      Engine.start(
        ingress_node,
        options[:ingress_options],
        options[:ingress_process_options]
      )

    {:ok, egress_engine} =
      Engine.start(
        egress_node,
        options[:egress_options],
        options[:egress_process_options]
      )

    sink = %Engine.Endpoint.DistributedSink{
      twin_node: egress_node,
      pair_id: id
    }

    source = %Engine.Endpoint.DistributedSource{
      twin_node: ingress_node,
      pair_id: id
    }

    Engine.add_endpoint(ingress_engine, sink)
    Engine.add_endpoint(egress_engine, source)

    {:ok, %{
      ingress_engine: ingress_engine,
      egress_engine: egress_engine,
      participants: %{}
     }}
  end

  @impl true
  def handle_info({:register, who}, state) do
    Registry.register(get_registry_name(), self(), who)
    {:ok, state}
  end

  @impl true
  def handle_info({:unregister, pid}, _ctx, state) do
    Registry.unregister_match(get_registry_name(), self(), pid)
    {:ok, state}
  end

  @impl true
  def handle_info({:media_event, from, data}, state) do
    case MediaEvent.deserialize(data) do
      {:ok, event} ->
        if event.type == :join or Map.has_key?(state.peers, from) do
          state = handle_media_event(event, from, state)
          {:noreply, state}
        else
          Membrane.Logger.warn("Received media event from unknown peer id: #{inspect(from)}")
          {:noreply, state}
        end

      {:error, :invalid_media_event} ->
        Membrane.Logger.warn("Invalid media event #{inspect(data)}")
          {:noreply, state}
    end
  end

  @impl true
  def handle_info({:webrtc_event, from, data}, state) do
    {:ok, state} = handle_webrtc_event(data, from, state)
    {:noreply, state}
  end

  def get_registry_name(), do: @registry_name

  defp handle_media_event(%{type: :webrtc_event} = event, from, state) do
    case event.engine_type do
      :ingress -> state.ingress_engine
      :egress -> state.egress_engine
    end
    |> Engine.receive_webrtc_event(event, from)

    {:ok, state}
  end

  defp handle_media_event(%{type: :join, data: data}, participant_id, state) do
    peer = %Engine.Peer{id: participant_id, metadata: data.metadata}
    ingress = state.ingress_engine
    egress = state.egress_engine

    Engine.add_peer(state.ingress_engine, peer)
    Engine.add_peer(state.egress_engine, peer)

    state =
      put_in(state, [:participants, participant_id], %{
        peer: peer,
        status: %{
          ingress: :added,
          egress: :added
        },
        participant_status_sent?: false,
        media_events_queue: []
      })

    # add participant to both, ingress and egress nodes
    {:ok, state}

  end

  defp handle_media_event(%{type: :leave}, peer_id, state) do
    # %Message.PeerLeft{rtc_engine: self(), peer: state.peers[peer_id]}
    # |> dispatch()

    # remove_peer(peer_id, ctx, state)
    {:ok, state}
  end

  defp handle_media_event(
         %{type: :update_participant_metadata, data: %{metadata: metadata}},
         peer_id,
         state
       ) do
    # peer = Map.get(state.peers, peer_id)

    # if peer.metadata != metadata do
    #   updated_peer = %{peer | metadata: metadata}
    #   state = put_in(state, [:peers, peer_id], updated_peer)

    #   MediaEvent.create_peer_updated_event(updated_peer)
    #   |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
    #   |> dispatch()

    #   {[], state}
    # else
    #   {[], state}
    # end
    {:ok, state}
  end

  defp handle_webrtc_event(%{type: :peer_accepted} = event, from, state) do
    # check if peer was accepted in both ingress and egress endpoints, eventually return
    # "perticipantAccepted"

    if Map.has_key?(state.participants, event.id) do
      where = if event == state.ingress_engine, do: :ingress, else: :egress
      state = put_in(state, [:participants, event.id, :status, where], :accepted)

      {:ok, state}
    else
      {:ok, state}
    end
  end

  defp handle_webrtc_event(%{type: :peer_denied} = event, from, state) do
    #  jw
    {:ok, state}
  end

  defp handle_webrtc_event(%{type: :peer_left} = event, from, state) do
    # should be preceded by receiving room_event like "participantLeft"
    {:ok, state}
  end

  defp handle_webrtc_event(%{type: :peer_updated} = event, from, state) do
    # as far as peer will not have it's metadata, this event makes no sense
    {:ok, state}
  end

  defp handle_webrtc_event(%{type: type} = event, from, state)
      when type in [:track_added, :track_removed, :track_updated] do
    # add participant

    # forward straight to user
    # but think about fact, that single track may be removed many times, one time on one engine
    # maybe send to user only messages from his egress node?
    # maybe on fronted send only messages only from his egress node?
    # but webrtc endpoint on ingress node shouldn't subscribe on any track, so there should be no issue
    {:ok, state}

  end

  defp handle_webrtc_event(%{type: :tracks_priority} = event, from, state) do
    # forward to user, maybe add info about participants
    {:ok, state}
  end

  defp handle_webrtc_event(%{type: :custom} = event, from, state) do
    # make sure, if this makes any sense
    # what is frontend implememtnation really doing with this? Does anything really uses this?
    # maybe there are sent all of messages like sdp
    # do custom media eventow nie doklejasz niczego ;P

    {:ok, state}
  end

  defp get_engine_name(engine, state) do
    if state.egress_engine == engine, do: :egress, else: :ingress
  end

  defp maybe_send_participant_accepted_or_denied(participant_id, state) do
    case state.participants[participant_id].status do
      %{ingress: :accepted, egress: :accepted} ->
        metadata = state.participants[participant_id].metadata
        send_participant_accepted(participant_id, metadata, state)
        put_in(state, [:participants, participant_id, :participant_status_sent?], true)

      %{ingress: :added, egress: :accepted} ->
        state

      %{ingress: :accepted, egress: :added} ->
        state

      _status ->
        metadata = state.participants[participant_id].metadata
        send_participant_denied(participant_id, metadata, state)
        remove_participant(id, state)
    end
  end

  defp remove_participant(id, state) do
    status = state.participants[participant_id].status

    if status.ingress in [:accepted, :added] do
      Engine.remove_peer(state.ingress_engine, participant_id)
    end

    if state.egress in [:accepted, :added] do
      Engine.remove_peer(state.egress_engine, participant_id)
    end

    {_participant, state} = pop_in(state, [:participants, participant_id])
    state
  end



  defp send_participant_denied(id, metadata, state) do

  end

  defp send_participant_accepted(id, metadata, state) do

  end




end
