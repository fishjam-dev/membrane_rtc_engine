defmodule Membrane.RTC.Engine do
  @moduledoc """
  RTC Engine implementation.

  RTC Engine is an abstraction layer responsible for linking together different types of `Endpoints`.
  From the implementation point of view, RTC Engine is a `Membrane.Pipeline`.

  ## Messages

  The RTC Engine works by sending messages which notify user logic about important events like
  "There is a new peer, would you like to to accept it?".
  To receive RTC Engine messages you have to register your process so that RTC Engine will
  know where to send them.
  All messages RTC Engine can emit are described in `#{inspect(__MODULE__)}.Message` docs.

  ### Registering for messages

  Registration can be done using `register/2` e.g.

  ```elixir
  Engine.register(rtc_engine, self())
  ```

  This will register your process to receive RTC Engine messages.
  If your process implements `GenServer` behavior then all messages can be handled
  by `c:GenServer.handle_info/2`, e.g.

  ```elixir
  @impl true
  def handle_info(%Message.NewPeer{rtc_engine: rtc_engine, peer: peer}, state) do
    Engine.accept_peer(rtc_engine, peer.id)
    {:noreply, state}
  end
  ```

  You can register multiple processes to receive messages from an RTC Engine instance.
  In such a case each message will be sent to each registered process.

  ## Client Libraries

  RTC Engine allows creating Client Libraries that can send and receive media tracks from it.
  The current version of RTC Engine ships with WebRTC Client Library which connects to the RTC Engine
  via WebRTC standard.
  Communication with Client Libraries is done using `Media Events`.
  Media Events are control messages which notify about e.g. new peer joining to the RTC Engine.
  When Client Library receives Media Event it can invoke some callbacks.
  In the case of WebRTC Client Library, these are e.g. `onPeerJoined` or `onTrackAdded`.
  When RTC Engine receives Media Event it can emit some messages e.g. `t:#{inspect(__MODULE__)}.Message.NewPeer.t/0`.
  More about Media Events can be read in subsequent sections.
  Below there is a figure showing the architecture of the RTC Engine working in conjunction with some Client Library.

  ```txt
      +--------------------------------- media events -----------------------------+
      |                                (signaling layer)                           |
      |                                                                            |
      |                                                                            |
  +--------+                 +---------+             +--------+               +---------+
  | user   | <-   media   -> | Client  |             |  RTC   | <- media   -> | user    |
  | client |      events     | Library | <- media -> | Engine |    events     | backend |
  | logic  | <- callbacks -  |         |             |        | - messages -> | logic   |
  +--------+                 +---------+             +--------+               +---------+
  ```



  ### Media Events

  Media Events are blackbox messages that carry data important for the
  RTC Engine and its Client Library, but not for the user.
  There are two types of Media Events:
  * Internal Media Events - generic, protocol-agnostic Media Events sent by RTC Engine itself.
  Example Internal Media Events are `peerJoined`, `peerLeft`, `tracksAdded` or `tracksRemoved`.
  * Custom Media Events - they can be used to send custom data from Client Library to some Endpoint inside RTC Engine
  and vice versa. In the case of WebRTC Client Library, these are `sdpOffer`, `sdpAnswer`, or `iceCandidate`.

  An application is obligated to transport Media Events from an RTC Engine instance to
  its Client Library, and vice versa.

  When the RTC Engine needs to send a Media Event to a specific client, registered processes will
  receive `t:#{inspect(__MODULE__)}.Message.MediaEvent.t/0` message with `to` field indicating where this Media Event
  should be sent to.
  This can be either `:broadcast`, when the event should be sent to all peers, or `peer_id`
  when the messages should be sent to the specified peer. The `event` is encoded in binary format,
  so it is ready to send without modification.

  Feeding an RTC Engine instance with Media Events from a Client Library can be done using `receive_media_event/2`.
  Assuming the user process is a GenServer, the Media Event can be received by `c:GenServer.handle_info/2` and
  conveyed to the RTC Engine in the following way:

  ```elixir
  @impl true
  def handle_info({:media_event, from, event} = msg, state) do
    Engine.receive_media_event(state.rtc_engine, from, event)
    {:noreply, state}
  end
  ```

  What is important, Membrane RTC Engine doesn't impose usage of any specific transport layer for carrying
  Media Events through the network.
  You can e.g. use Phoenix and its channels.
  This can look like this:

  ```elixir
  @impl true
  def handle_in("mediaEvent", %{"data" => event}, socket) do
    Engine.receive_media_event(socket.assigns.room, socket.assigns.peer_id, event)
    {:noreply, socket}
  end
  ```

  ## Peers

  Each peer represents some user that can possess some metadata.
  A Peer can be added in two ways:
  * by sending proper Media Event from a Client Library
  * using `add_peer/3`

  Adding a peer will cause RTC Engine to emit Media Event which will notify connected clients about new peer.

  ### Peer id

  Peer ids must be assigned by application code. This is not done by the RTC Engine or its client library.
  Ids can be assigned when a peer initializes its signaling layer.

  ## Endpoints

  `Endpoints` are `Membrane.Bin`s able to publish their own tracks and subscribe for tracks from other Endpoints.
  One can think about Endpoint as an entity responsible for handling some specific task.
  An Endpoint can be added and removed using `add_endpoint/3` and `remove_endpoint/2` respectively.

  There are two types of Endpoints:
  * Standalone Endpoints - they are in most cases spawned only once per RTC Engine instance and they are not associated with any peer.
  * Peer Endpoints - they are associated with some peer.
  Associating Endpoint with Peer will cause RTC Engine to send some Media Events to the Enpoint's Client Library
  e.g. one which indicates which tracks belong to which peer.

  Currently RTC Engine ships with the implementation of two Endpoints:
  * `#{inspect(__MODULE__)}.Endpoint.WebRTC` which is responsible for establishing a connection with some WebRTC
  peer (mainly browser) and exchanging media with it. WebRTC Endpoint is a Peer Endpoint.
  * `#{inspect(__MODULE__)}.Endpoint.HLS` which is responsible for receiving media tracks from all other Endpoints and
  saving them to files by creating HLS playlists. HLS Endpoint is a Standalone Endpoint.

  User can also implement custom Endpoints.

  ### Implementing custom RTC Engine Endpoint

  Each RTC Engine Endpoint has to:
  * implement `Membrane.Bin` behavior
  * specify input, output, or both input and output pads depending on what it is intended to do.
  For example, if Endpoint will not publish any tracks but only subscribe for tracks from other Endpoints it can specify only input pads.
  Pads should have the following form

  ```elixir
    def_input_pad :input,
      demand_unit: :buffers,
      caps: <caps>,
      availability: :on_request

    def_output_pad :output,
      demand_unit: :buffers,
      caps: <caps>,
      availability: :on_request
  ```

  Where `caps` are `t:Membrane.Caps.t/0` or `:any`.

  * publish or subscribe for some tracks using actions `t:publish_action_t/0` or `t:subscribe_action_t/0` respectively.
  The first will cause RTC Engine to send a message in form of `{:new_tracks, tracks}`
  where `tracks` is a list of `t:#{inspect(__MODULE__)}.Track.t/0` to all other Endpoints.
  When an Endpoint receives such a message it can subscribe for new tracks by returning action `t:subscribe_action_t/0`.
  An Endpoint will be notified about track readiness it subscribed for in `c:Membrane.Bin.handle_pad_added/3` callback.
  An example implementation of `handle_pad_added` callback can look like this

  ```elixir
    @impl true
    def handle_pad_added(Pad.ref(:input, _track_id) = pad, _ctx, state) do
      links = [
        link_bin_input(pad)
        |> via_in(pad)
        |> to(:my_element)
      ]

      {{:ok, spec: %ParentSpec{links: links}}, state}
    end
  ```

  Where `:my_element` is a custom Membrane element responsible for processing track.

  Endpoint will be also notified when some tracks it subscribed for are removed with
  `{:removed_tracks, tracks}` message where `tracks` is a list of `t:#{inspect(__MODULE__)}.Track.t/0`.
  """
  use Membrane.Pipeline
  use OpenTelemetryDecorator
  import Membrane.RTC.Utils

  alias Membrane.RTC.Engine.{Endpoint, MediaEvent, Message, Track, Peer, TrackManager, EngineTee}

  require Membrane.Logger

  @registry_name Membrane.RTC.Engine.Registry.Dispatcher

  @typedoc """
  RTC Engine configuration options.

  `id` is used by logger. If not provided it will be generated.
  `trace_ctx` is used by OpenTelemetry. All traces from this engine will be attached to this context.
  Example function from which you can get Otel Context is `get_current/0` from `OpenTelemetry.Ctx`.
  `track_manager?` decide if `#{inspect(__MODULE__)}.TrackManager` should be spawned for engine.


  """
  @type options_t() :: [
          id: String.t(),
          trace_ctx: map(),
          track_manager?: boolean()
        ]

  @typedoc """
  Endpoint configuration options.

  * `peer_id` - associate endpoint with exisiting peer
  * `endpoint_id` - assign endpoint id. If not provided it will be generated by RTC Engine. This option cannot be used together with `peer_id`.
  Endpoints associated with peers have the id `peer_id`.
  * `node` - node on which endpoint should be spawned. If not provided, current node is used.
  """
  @type endpoint_options_t() :: [
          endpoint_id: String.t(),
          peer_id: String.t(),
          node: node()
        ]

  @typedoc """
  Membrane action that will cause RTC Engine to publish some message to all other endpoints.
  """
  @type publish_action_t() :: {:notify, {:publish, publish_message_t()}}

  @typedoc """
  Membrane action that make subscribtion for tracks in given format.

  Endpoint  will be notified about track readiness in `c:Membrane.Bin.handle_pad_added/3` callback.
  `tracks` is a list in form of pairs `{track_id, track_format}`, where `track_id` is id of track this endpoint subscribes for
  and `track_format` is the format of track that this endpoint is willing to receive.
  If `track_format` is `:raw` Endpoint will receive track in `t:#{inspect(__MODULE__)}.Track.encoding/0` format.
  """
  @type subscribe_action_t() :: {:notify, {:subscribe, tracks :: [{Track.id(), Track.format()}]}}

  @typedoc """
  Membrane action that will inform RTC Engine about track readiness.
  """
  @type track_ready_action_t() ::
          {:notify,
           {:track_ready, Track.id(), Track.encoding(),
            depayloading_filter :: Membrane.ParentSpec.child_spec_t()}}

  @typedoc """
  Membrane action that will generate Custom Media Event.
  """
  @type custom_media_event_action_t() :: {:notify, {:custom_media_event, data :: binary()}}

  @typedoc """
  Types of messages that can be published to other Endpoints.
  """
  @type publish_message_t() :: {:new_tracks, [Track.t()]} | {:removed_tracks, [Track.t()]}

  @spec start(options :: options_t(), process_options :: GenServer.options()) ::
          GenServer.on_start()
  def start(options, process_options) do
    do_start(:start, options, process_options)
  end

  @spec start_link(options :: options_t(), process_options :: GenServer.options()) ::
          GenServer.on_start()
  def start_link(options, process_options) do
    do_start(:start_link, options, process_options)
  end

  defp do_start(func, options, process_options) when func in [:start, :start_link] do
    id = options[:id] || "#{UUID.uuid4()}"
    track_manager? = options[:track_manager?] || false
    options = Keyword.put(options, :id, id)
    options = Keyword.put(options, :track_manager?, track_manager?)

    Membrane.Logger.info("Starting a new RTC Engine instance with id: #{id}")

    apply(Membrane.Pipeline, func, [
      __MODULE__,
      options,
      process_options
    ])
  end

  @spec get_registry_name() :: atom()
  def get_registry_name(), do: @registry_name

  @doc """
  Adds endpoint to the RTC Engine

  Returns `:error` when there are both `peer_id` and `endpoint_id` specified in `opts`.
  For more information refer to `t:endpoint_options_t/0`.
  """
  @spec add_endpoint(
          pid :: pid(),
          endpoint :: Membrane.ParentSpec.child_spec_t(),
          opts :: endpoint_options_t()
        ) :: :ok | :error
  def add_endpoint(pid, endpoint, opts) do
    if Keyword.has_key?(opts, :endpoint_id) and
         Keyword.has_key?(opts, :peer_id) do
      raise "You can't pass both option endpoint_id and peer_id"
    else
      send(pid, {:add_endpoint, endpoint, opts})
      :ok
    end
  end

  @doc """
  Removes endpoint from the RTC Engine
  """
  @spec remove_endpoint(
          pid :: pid(),
          id :: String.t()
        ) :: :ok
  def remove_endpoint(rtc_engine, id) do
    send(rtc_engine, {:remove_endpoint, id})
    :ok
  end

  @doc """
  Adds peer to the RTC Engine
  """
  @spec add_peer(pid :: pid(), peer :: Peer.t()) :: :ok
  def add_peer(pid, peer) do
    send(pid, {:add_peer, peer})
    :ok
  end

  @doc """
  Removes peer from RTC Engine.
  """
  @spec remove_peer(rtc_engine :: pid(), peer_id :: any()) :: :ok
  def remove_peer(rtc_engine, peer_id) do
    send(rtc_engine, {:remove_peer, peer_id})
    :ok
  end

  @doc """
  Allows peer for joining to the RTC Engine
  """
  @spec accept_peer(
          pid :: pid(),
          peer_id :: String.t()
        ) :: :ok
  def accept_peer(pid, peer_id) do
    send(pid, {:accept_new_peer, peer_id})
    :ok
  end

  @doc """
  Deny peer from joining to the RTC Engine.
  """
  @spec deny_peer(pid :: pid(), peer_id :: String.t()) :: :ok
  def deny_peer(pid, peer_id) do
    send(pid, {:deny_new_peer, peer_id})
    :ok
  end

  @doc """
  The same as `deny_peer/2` but allows for passing any data that will be returned to the client.

  This can be used for passing reason of peer refusal.
  """
  @spec deny_peer(pid :: pid(), peer_id :: String.t(), data: any()) :: :ok
  def deny_peer(pid, peer_id, data) do
    send(pid, {:deny_new_peer, peer_id, data})
    :ok
  end

  @doc """
  Registers process with pid `who` for receiving messages from RTC Engine
  """
  @spec register(rtc_engine :: pid(), who :: pid()) :: :ok
  def register(rtc_engine, who \\ self()) do
    send(rtc_engine, {:register, who})
    :ok
  end

  @doc """
  Unregisters process with pid `who` from receiving messages from RTC Engine
  """
  @spec unregister(rtc_engine :: pid(), who :: pid()) :: :ok
  def unregister(rtc_engine, who \\ self()) do
    send(rtc_engine, {:unregister, who})
    :ok
  end

  @doc """
  Sends Media Event to RTC Engine.
  """
  @spec receive_media_event(rtc_engine :: pid(), media_event :: {:media_event, pid(), any()}) ::
          :ok
  def receive_media_event(rtc_engine, media_event) do
    send(rtc_engine, media_event)
    :ok
  end

  @impl true
  def handle_init(options) do
    play(self())

<<<<<<< HEAD
    trace_ctx =
      if Keyword.has_key?(options, :trace_ctx) do
        OpenTelemetry.Ctx.attach(options[:trace_ctx])
      else
        Membrane.RTC.Utils.create_otel_context("rtc:#{options[:id]}")
=======
    trace_ctx = create_otel_context("rtc:#{options[:id]}")

    track_manager =
      if options[:track_manager?] do
        {:ok, pid} = TrackManager.start_link(ets_name: options[:id], engine: self())
        pid
      else
        nil
>>>>>>> d3bc34a (Wip RTCEngineTee)
      end

    {{:ok, log_metadata: [rtc: options[:id]]},
     %{
       id: options[:id],
       component_path: Membrane.ComponentPath.get_formatted(),
       trace_context: trace_ctx,
       peers: %{},
       endpoints: %{},
       waiting_for_linking: %{},
       filters: %{},
       subscriptions: %{},
       track_manager: track_manager
     }}
  end

  @impl true
  @decorate trace("engine.other.register", include: [[:state, :id]])
  def handle_other({:register, pid}, _ctx, state) do
    Registry.register(get_registry_name(), self(), pid)
    {:ok, state}
  end

  @impl true
  @decorate trace("engine.other.unregister", include: [[:state, :id]])
  def handle_other({:unregister, pid}, _ctx, state) do
    Registry.unregister_match(get_registry_name(), self(), pid)
    {:ok, state}
  end

  @impl true
  @decorate trace("engine.other.tracks_priority", include: [[:state, :id]])
  def handle_other({:track_priorities, endpoint_to_tracks}, ctx, state) do
    _msgs =
      Enum.map(endpoint_to_tracks, fn {{:endpoint, endpoint_id}, tracks} ->
        MediaEvent.create_tracks_priority_event(tracks)
        |> then(&%Message.MediaEvent{rtc_engine: self(), to: endpoint_id, data: &1})
        |> dispatch()
      end)

    tee_actions =
      ctx
      |> filter_children(pattern: {:tee, _tee_name})
      |> Enum.flat_map(&[forward: {&1, :track_priorities_updated}])

    {{:ok, tee_actions}, state}
  end

  @impl true
  @decorate trace("engine.other.remove_peer", include: [[:state, :id]])
  def handle_other({:remove_peer, id}, ctx, state) do
    {actions, state} = remove_peer(id, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  @decorate trace("engine.other.add_endpoint", include: [[:state, :component_path], [:state, :id]])
  def handle_other({:add_endpoint, endpoint, opts}, _ctx, state) do
    peer_id = opts[:peer_id]
    endpoint_id = opts[:endpoint_id] || opts[:peer_id] || "#{UUID.uuid4()}"

    cond do
      Map.has_key?(state.endpoints, endpoint_id) ->
        Membrane.Logger.warn(
          "Cannot add Endpoint with id #{inspect(endpoint_id)} as it already exists"
        )

        {:ok, state}

      peer_id != nil and !Map.has_key?(state.peers, peer_id) ->
        Membrane.Logger.warn(
          "Cannot attach Endpoint to peer with id #{peer_id} as such peer does not exist"
        )

        {:ok, state}

      true ->
        {actions, state} = setup_endpoint(endpoint, opts, state)
        {{:ok, actions}, state}
    end
  end

  @impl true
  @decorate trace("engine.other.add_peer", include: [[:state, :id]])
  def handle_other({:add_peer, peer}, _ctx, state) do
    {actions, state} = do_accept_new_peer(peer, state)
    {{:ok, actions}, state}
  end

  @impl true
  @decorate trace("engine.other.remove_endpoint", include: [[:state, :id]])
  def handle_other({:remove_endpoint, id}, ctx, state) do
    case(do_remove_endpoint(id, ctx, state)) do
      {:absent, [], state} ->
        Membrane.Logger.info("Endpoint #{inspect(id)} already removed")
        {:ok, state}

      {:present, actions, state} ->
        {{:ok, actions}, state}
    end
  end

  @impl true
  @decorate trace("engine.other.media_event", include: [[:state, :id]])
  def handle_other({:media_event, from, data}, ctx, state) do
    case MediaEvent.deserialize(data) do
      {:ok, event} ->
        if event.type == :join or Map.has_key?(state.peers, from) do
          {actions, state} = handle_media_event(event, from, ctx, state)
          {{:ok, actions}, state}
        else
          Membrane.Logger.warn("Received media event from unknown peer id: #{inspect(from)}")
          {:ok, state}
        end

      {:error, :invalid_media_event} ->
        Membrane.Logger.warn("Invalid media event #{inspect(data)}")
        {:ok, state}
    end
  end

  defp handle_media_event(%{type: :join, data: data}, peer_id, _ctx, state) do
    peer = Peer.new(peer_id, data.metadata || %{})
    dispatch(%Message.NewPeer{rtc_engine: self(), peer: peer})

    receive do
      {:accept_new_peer, ^peer_id} ->
        do_accept_new_peer(peer, state)

      {:accept_new_peer, peer_id} ->
        Membrane.Logger.warn("Unknown peer id passed for acceptance: #{inspect(peer_id)}")
        {[], state}

      {:deny_new_peer, peer_id} ->
        MediaEvent.create_peer_denied_event()
        |> then(&%Message.MediaEvent{rtc_engine: self(), to: peer_id, data: &1})
        |> dispatch()

        {[], state}

      {:deny_new_peer, peer_id, data: data} ->
        MediaEvent.create_peer_denied_event(data)
        |> then(&%Message.MediaEvent{rtc_engine: self(), to: peer_id, data: &1})
        |> dispatch()

        {[], state}
    end
  end

  defp handle_media_event(%{type: :custom, data: event}, peer_id, ctx, state) do
    actions = forward({:endpoint, peer_id}, {:custom_media_event, event}, ctx)

    {actions, state}
  end

  defp handle_media_event(%{type: :leave}, peer_id, ctx, state) do
    %Message.PeerLeft{rtc_engine: self(), peer: state.peers[peer_id]}
    |> dispatch()

    remove_peer(peer_id, ctx, state)
  end

  defp handle_media_event(
         %{type: :update_peer_metadata, data: %{metadata: metadata}},
         peer_id,
         _ctx,
         state
       ) do
    peer = Map.get(state.peers, peer_id)

    if peer.metadata != metadata do
      updated_peer = %{peer | metadata: metadata}
      state = put_in(state, [:peers, peer_id], updated_peer)

      MediaEvent.create_peer_updated_event(updated_peer)
      |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
      |> dispatch()

      {[], state}
    else
      {[], state}
    end
  end

  defp handle_media_event(
         %{
           type: :update_track_metadata,
           data: %{track_id: track_id, track_metadata: track_metadata}
         },
         endpoint_id,
         _ctx,
         state
       ) do
    endpoint = Map.get(state.endpoints, endpoint_id)
    track = Endpoint.get_track_by_id(endpoint, track_id)

    if track != nil and track.metadata != track_metadata do
      endpoint = Endpoint.update_track_metadata(endpoint, track_id, track_metadata)
      state = put_in(state, [:endpoints, endpoint_id], endpoint)

      MediaEvent.create_track_updated_event(endpoint_id, track_id, track_metadata)
      |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
      |> dispatch()

      {[], state}
    else
      {[], state}
    end
  end

  @impl true
  @decorate trace("engine.notification.custom_media_event", include: [[:state, :id]])
  def handle_notification({:custom_media_event, data}, {:endpoint, peer_id}, _ctx, state) do
    MediaEvent.create_custom_event(data)
    |> then(&%Message.MediaEvent{rtc_engine: self(), to: peer_id, data: &1})
    |> dispatch()

    {:ok, state}
  end

  # NOTE: When `payload_and_depayload_tracks?` options is set to false we may still want to depayload
  # some streams just in one place to e.g. dump them to HLS or perform any actions on depayloaded
  # media without adding payload/depaload elements to all EndpointBins (performing unnecessary work).
  #
  # To do that one just need to apply `depayloading_filter` after the tee element on which filter's the notification arrived.
  @impl true
  @decorate trace("engine.notification.track_ready",
              include: [:track_id, :encoding, [:state, :id]]
            )
  def handle_notification(
        {:track_ready, track_id, encoding, depayloading_filter},
        endpoint_bin_name,
        ctx,
        state
      ) do
    Membrane.Logger.info(
      "New incoming #{encoding} track #{track_id} from #{inspect(endpoint_bin_name)}"
    )

    {:endpoint, endpoint_id} = endpoint_bin_name

    endpoint_track_ids = {endpoint_id, track_id}
    endpoint_tee = {:tee, endpoint_track_ids}
    fake = {:fake, endpoint_track_ids}
    filter = {:filter, endpoint_track_ids}
    filter_tee = {:filter_tee, endpoint_track_ids}
    filter_tee_fake = {:filter_tee_fake, endpoint_track_ids}

    track = state.endpoints |> Map.get(endpoint_id) |> Endpoint.get_track_by_id(track_id)

    children = %{
      endpoint_tee =>
        if state.track_manager != nil do
          %EngineTee{ets_name: state.id, track_id: track_id, type: track.type}
        else
          Membrane.Element.Tee.Master
        end,
      fake => Membrane.Element.Fake.Sink.Buffers,
      filter => depayloading_filter,
      filter_tee => Membrane.Element.Tee.Master,
      filter_tee_fake => Membrane.Element.Fake.Sink.Buffers
    }

    link_to_fake =
      link(endpoint_bin_name)
      |> via_out(Pad.ref(:output, track_id))
      |> to(endpoint_tee)
      |> via_out(:master)
      |> to(fake)

    filter_link =
      link(endpoint_tee)
      |> via_out(:copy)
      |> to(filter)
      |> to(filter_tee)
      |> via_out(:master)
      |> to(filter_tee_fake)

    {links, waiting_for_linking} =
      link_inbound_track(
        track_id,
        endpoint_tee,
        filter_tee,
        state.waiting_for_linking,
        ctx,
        state
      )

    spec = %ParentSpec{
      children: children,
      links: [link_to_fake, filter_link | links],
      crash_group: {endpoint_id, :temporary}
    }

    state =
      update_in(
        state,
        [:endpoints, endpoint_id],
        &Endpoint.update_track_encoding(&1, track_id, encoding)
      )

    state = %{state | waiting_for_linking: waiting_for_linking}
    state = put_in(state, [:filters, track_id], filter)

    {{:ok, spec: spec}, state}
  end

  @impl true
  @decorate trace("engine.notification.subscribe", include: [:endpoint_id, [:state, :id]])
  def handle_notification(
        {:subscribe, tracks},
        {:endpoint, endpoint_id},
        ctx,
        state
      ) do
    {new_links, new_waiting_for_linking} = link_outbound_tracks(tracks, endpoint_id, ctx)

    state =
      update_in(
        state,
        [:waiting_for_linking, endpoint_id],
        &MapSet.union(&1, new_waiting_for_linking)
      )

    new_endpoint_subscriptions = Map.new(tracks, fn {track_id, format} -> {track_id, format} end)

    subscriptions =
      Map.update(
        state.subscriptions,
        endpoint_id,
        new_endpoint_subscriptions,
        &Map.merge(&1, new_endpoint_subscriptions)
      )

    state = %{state | subscriptions: subscriptions}
    {{:ok, [spec: %ParentSpec{links: new_links}]}, state}
  end

  @impl true
  @decorate trace("engine.notification.publish.new_tracks",
              include: [:endpoint_id, [:state, :id]]
            )
  def handle_notification(
        {:publish, {:new_tracks, tracks}},
        {:endpoint, endpoint_id},
        ctx,
        state
      ) do
    id_to_track = Map.new(tracks, &{&1.id, &1})

    state =
      update_in(state, [:endpoints, endpoint_id, :inbound_tracks], &Map.merge(&1, id_to_track))

    tracks_msgs =
      do_publish(
        state.endpoints,
        ctx,
        {:new_tracks, tracks},
        {:endpoint, endpoint_id}
      )

    endpoint = get_in(state, [:endpoints, endpoint_id])
    track_id_to_track_metadata = Endpoint.get_track_id_to_metadata(endpoint)

    MediaEvent.create_tracks_added_event(endpoint_id, track_id_to_track_metadata)
    |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
    |> dispatch()

    {{:ok, tracks_msgs}, state}
  end

  @impl true
  @decorate trace("engine.notification.publish.removed_tracks",
              include: [:endpoint_id, [:state, :id]]
            )
  def handle_notification(
        {:publish, {:removed_tracks, tracks}},
        {:endpoint, endpoint_id},
        ctx,
        state
      ) do
    id_to_track = Map.new(tracks, &{&1.id, &1})

    state =
      update_in(state, [:endpoints, endpoint_id, :inbound_tracks], &Map.merge(&1, id_to_track))

    tracks_msgs =
      do_publish(
        state.endpoints,
        ctx,
        {:remove_tracks, tracks},
        {:endpoint, endpoint_id}
      )

    track_ids = Enum.map(tracks, & &1.id)

    MediaEvent.create_tracks_removed_event(endpoint_id, track_ids)
    |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
    |> dispatch()

    {{:ok, tracks_msgs}, state}
  end

  defp link_inbound_track(track_id, tee, filter_tee, waiting_for_linking, ctx, state) do
    reduce_children(ctx, {[], waiting_for_linking}, fn
      {:endpoint, endpoint_id} = endpoint_name, {new_links, waiting_for_linking} ->
        if MapSet.member?(waiting_for_linking[endpoint_id], track_id) do
          format = state.subscriptions[endpoint_id][track_id]

          format_specific_tee = if format == :raw, do: filter_tee, else: tee

          new_link =
            link(format_specific_tee)
            |> via_out(Pad.ref(:copy, endpoint_name))
            |> via_in(Pad.ref(:input, track_id))
            |> to(endpoint_name)

          waiting_for_linking =
            Map.update!(waiting_for_linking, endpoint_id, &MapSet.delete(&1, track_id))

          {new_links ++ [new_link], waiting_for_linking}
        else
          {new_links, waiting_for_linking}
        end

      _other_child, {new_links, waiting_for_linking} ->
        {new_links, waiting_for_linking}
    end)
  end

  defp link_outbound_tracks(tracks, endpoint_id, ctx) do
    Enum.reduce(tracks, {[], MapSet.new()}, fn
      {track_id, format}, {new_links, not_linked} ->
        format_specific_tee =
          if format == :raw do
            find_child(ctx, pattern: {:filter_tee, {_other_endpoint_id, ^track_id}})
          else
            find_child(ctx, pattern: {:tee, {_other_endpoint_id, ^track_id}})
          end

        if format_specific_tee do
          endpoint_name = {:endpoint, endpoint_id}

          new_link =
            link(format_specific_tee)
            |> via_out(Pad.ref(:copy, endpoint_name))
            |> via_in(Pad.ref(:input, track_id))
            |> to({:endpoint, endpoint_id})

          {new_links ++ [new_link], not_linked}
        else
          {new_links, MapSet.put(not_linked, track_id)}
        end

      _track, {new_links, not_linked} ->
        {new_links, not_linked}
    end)
  end

  defp dispatch(msg) do
    Registry.dispatch(get_registry_name(), self(), fn entries ->
      for {_, pid} <- entries, do: send(pid, msg)
    end)
  end

  defp do_accept_new_peer(peer, state) do
    if Map.has_key?(state.peers, peer.id) do
      Membrane.Logger.warn("Peer with id: #{inspect(peer.id)} has already been added")
      {[], state}
    else
      state = put_in(state, [:peers, peer.id], peer)

      MediaEvent.create_peer_accepted_event(
        peer.id,
        Map.delete(state.peers, peer.id),
        state.endpoints
      )
      |> then(&%Message.MediaEvent{rtc_engine: self(), to: peer.id, data: &1})
      |> dispatch()

      MediaEvent.create_peer_joined_event(peer)
      |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
      |> dispatch()

      {[], state}
    end
  end

  defp setup_endpoint(endpoint_entry, opts, state) do
    inbound_tracks = []
    outbound_tracks = get_outbound_tracks(state.endpoints)

    endpoint_id = opts[:endpoint_id] || opts[:peer_id]
    endpoint = Endpoint.new(endpoint_id, inbound_tracks)

    endpoint_name = {:endpoint, endpoint_id}

    children = %{
      endpoint_name => endpoint_entry
    }

    action = [
      forward: {endpoint_name, {:track_manager, state.track_manager}},
      forward: {endpoint_name, {:new_tracks, outbound_tracks}}
    ]

    state = put_in(state, [:waiting_for_linking, endpoint_id], MapSet.new())

    spec = %ParentSpec{
      node: opts[:node],
      children: children,
      crash_group: {endpoint_id, :temporary}
    }

    state = put_in(state.endpoints[endpoint_id], endpoint)

    {[spec: spec] ++ action, state}
  end

  defp get_outbound_tracks(endpoints),
    do: Enum.flat_map(endpoints, fn {_id, endpoint} -> Endpoint.get_tracks(endpoint) end)

  defp remove_peer(peer_id, ctx, state) do
    case do_remove_peer(peer_id, ctx, state) do
      {:absent, [], state} ->
        Membrane.Logger.info("Peer #{inspect(peer_id)} already removed")
        {[], state}

      {:present, actions, state} ->
        MediaEvent.create_peer_left_event(peer_id)
        |> then(&%Message.MediaEvent{rtc_engine: self(), to: :broadcast, data: &1})
        |> dispatch()

        send_if_not_nil(state.track_manager, {:unregister_endpoint, {:endpoint, peer_id}})

        {actions, state}
    end
  end

  defp do_remove_peer(peer_id, ctx, state) do
    if Map.has_key?(state.peers, peer_id) do
      {_peer, state} = pop_in(state, [:peers, peer_id])
      {_status, actions, state} = do_remove_endpoint(peer_id, ctx, state)
      {_waiting, state} = pop_in(state, [:waiting_for_linking, peer_id])
      {:present, actions, state}
    else
      {:absent, [], state}
    end
  end

  defp do_remove_endpoint(peer_id, ctx, state) do
    if Map.has_key?(state.endpoints, peer_id) do
      {endpoint, state} = pop_in(state, [:endpoints, peer_id])
      tracks = Enum.map(Endpoint.get_tracks(endpoint), &%Track{&1 | active?: true})

      tracks_msgs =
        do_publish(
          state.endpoints,
          ctx,
          {:remove_tracks, tracks},
          {:endpoint, peer_id}
        )

      endpoint_bin = ctx.children[{:endpoint, peer_id}]

      actions =
        if endpoint_bin == nil or endpoint_bin.terminating? do
          []
        else
          [remove_child: find_children_for_endpoint(endpoint, peer_id, ctx)]
        end

      {:present, tracks_msgs ++ actions, state}
    else
      {:absent, [], state}
    end
  end

  defp find_children_for_endpoint(endpoint, peer_id, ctx) do
    children =
      Endpoint.get_tracks(endpoint)
      |> Enum.map(fn track -> track.id end)
      |> Enum.flat_map(
        &[
          tee: {peer_id, &1},
          fake: {peer_id, &1},
          filter: {peer_id, &1},
          filter_tee: {peer_id, &1},
          filter_tee_fake: {peer_id, &1}
        ]
      )
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    [endpoint: peer_id] ++ children
  end

  defp do_publish(_endpoints, _ctx, {_, []} = _tracks, _endpoint_bin), do: []

  defp do_publish(endpoints, ctx, msg, endpoint_bin_name) do
    flat_map_children(ctx, fn
      {:endpoint, endpoint_id} = other_endpoint_bin ->
        endpoint = Map.get(endpoints, endpoint_id)

        if other_endpoint_bin != endpoint_bin_name and not is_nil(endpoint) do
          [forward: {other_endpoint_bin, msg}]
        else
          []
        end

      _child ->
        []
    end)
  end
end
