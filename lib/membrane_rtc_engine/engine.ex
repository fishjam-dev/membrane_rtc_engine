defmodule Membrane.RTC.Engine do
  @moduledoc """
  RTC Engine implementation.

  One RTC Engine instance is responsible for managing one room in which
  all tracks of one peer are forwarded to all other peers.

  The RTC Engine works by sending and receiving messages.
  All messages are described below.
  To receive RTC Engine messages you have to register your process so that RTC Engine will
  know where to send its messages.

  ## Registering for messages

  Registration can be done by using function register from this module, e.g.

  ```elixir
  Engine.register(rtc_pid, self())
  ```

  This will register your process to receive RTC Engine messages.
  If your process implements `GenServer` behaviour then all messages will be handled
  by `c:GenServer.handle_info/2`, e.g.

  ```elixir
  @impl true
  def handle_info({_rtc_engine, {:rtc_media_event, :broadcast, event}}, state) do
    for {_peer_id, pid} <- state.peer_channels, do: send(pid, {:media_event, event})
    {:noreply, state}
  end
  ```

  You can register multiple processes to receive messages from an RTC Engine instance.
  In such a case each message will be sent to each registered process.

  ## Media Events

  The RTC Engine needs to communicate with Membrane client libraries.
  This communication is done via `Media Event` messages.
  Media Events are blackbox messages that carry data important for the
  RTC Engine and client libraries, but not for the user. Example Media Events are
  SDP offers, ICE candidates, and information about new peers.

  An application is obligated to transport Media Events from an RTC Engine instance to
  its client library, and vice versa.

  When an RTC Engine needs to send a message to a specific client, registered processes will
  receive `{:rtc_media_event, to, event}`, where `to` specifies the message destination.
  This can be either `:broadcast`, when the event should be sent to all peers, or `peer_id`
  when the messages should be sent to specified peer. The `event` is encoded in binary format,
  so it is ready to send without modification.

  Feeding an RTC Engine instance with Media Events from a client library can be done by sending the
  message `{:media_event, from, event}`. Assuming the user process is a GenServer, the
  Media Event can be received by `c:GenServer.handle_info/2` and conveyed to the RTC Engine in
  the following way:

  ```elixir
  @impl true
  def handle_info({:media_event, _from, _event} = msg, state) do
    send(state.rtc_engine, msg)
    {:noreply, state}
  end
  ```

  What is important, Membrane RTC Engine doesn't impose usage of any specific transport layer.
  You can e.g. use Phoenix and its channels.
  This can look like this:

  ```elixir
  @impl true
  def handle_in("mediaEvent", %{"data" => event}, socket) do
    send(socket.assigns.room, {:media_event, socket.assigns.peer_id, event})

    {:noreply, socket}
  end
  ```

  ## Messages

  Each message the RTC Engine sends is a two-element tuple `{rtc_pid, msg}` where
  `rtc_pid` is the pid of the RTC Engine instance that sent message, and `msg` can be any data.

  Notice that thanks to presence of `rtc_pid` you can create multiple RTC Engine instances.

  Example RTC Engine message:

  ```elixir
  {_rtc_pid, {:vad_notification, val, peer_id}}
  ```

  #### RTC Engine sends following messages

  * `{:rtc_media_event, to, event}` - a Media Event that should be transported to the client
  library. When `from` is `:broadcast`, the Media Event should be sent to all peers. When
  `from` is a `peer_id`, the Media Event should be sent to that specified peer.
  * `{:vad_notification, val, peer_id}` - sent when peer with id `peer_id` is speaking.
  `VAD` stands for `Voice Activity Detection`. When `val` is `true` marks start of speech
  whereas `false` marks end of speech.
  * `{:new_peer, peer_id, metadata, track_metadata}` - sent when a new peer tries to join
  to an RTC Engine instance. `metadata` is any data passed by the client library while joining.
  `track_metadata` is a map where key is track id and value is its metadata defined in client
  library while adding a new track.
  * `{:peer_left, peer_id}` - sent when the peer with `peer_id` leaves an RTC Engine instance

  #### RTC Engine receives following messages

  * `{:register, pid}` - register given `pid` for receiving RTC Engine messages
  * `{:unregister, pid}` - unregister given `pid` from receiving RTC Engine messages
  * `{:media_event, from, event}` - feed Media Event to SFU. `from` is id of peer
  that this Media event comes from.
  * `{:accept_new_peer, peer_id, endpoint, node}` - accepts peer with id `peer_id`,
    with endpoint implementation pass as third parameter and node on which this peer should run
    (it is important when run in distributed environment)
  * `{:deny_new_peer, peer_id}` - denies peer with id `peer_id`
  * `{:remove_peer, peer_id}` - removes peer with id `peer_id`

  ## Endpoints

  RTC engine allow to pass custom implementation of endpoint. Endpoint is element which is able to link to with others elements.
  In most common case Endpoint is wrapper on some bin e.g HLS_endpoint is using sink_bin and
  Webrtc_endpoint is using Endpoint_bin from webrtc_plugin.

  It is possible add endpont outside of mechanism to :accept_new_peer e.g.
  ```elixir
    Engine.add_endpoint(rtc_pid,endpoint_id,endpoint)
  ```

  #### How to implement your custom endpoint

  To receive tracks from rtc_engine your endpoint have to reply on message `{:new_tracks, tracks}` with subscription notification.
  To generate it you can use subscribe function from this module.

  To send tracks to rtc_engine your endpoint have to send two messages to rtc_engine.
  * First is `{:new_tracks, tracks}` publish notification. Which inform that this endpoint want to send this tracks.
  * Second is `{:track_ready, track_id, encoding, depayloading_filter}` message. It inform that first packets of
  track with this id and this encoding arrived. Also you have to pass depayloading filter element which is responsible for
  convert this track in his specific format to raw.

  To create publish notification you can use publish function from this module.

  If you set that your endpoint should receive_media it will receive this messages:
  * `{:new_tracks, tracks}` - it will be received when some peer announce that it will send new tracks
  * `{:removed_tracks, tracks}` - it will be received when e.g some peer left room

  ## Peer id

  Peer ids must be assigned by application code. This is not done by the RTC Engine or its client library.
  Ids can be assigned when a peer initializes its signaling channel.

  Assuming we use a Phoenix channel as signaling layer:

  ```elixir
  def join("room:" <> room_id, _params, socket) do
    # ...
    peer_id = UUID.uuid4()
    {:ok, assign(socket, %{room_id: room_id, room: room, peer_id: peer_id})}
  end
  ```
  """
  use Membrane.Pipeline
  import Membrane.RTC.Utils

  alias Membrane.ICE.TurnUtils
  alias Membrane.RTC.Engine.{MediaEvent, Endpoint, Track}

  require Membrane.Logger

  @registry_name Membrane.RTC.Engine.Registry.Dispatcher

  @type stun_server_t() :: ExLibnice.stun_server()
  @type turn_server_t() :: ExLibnice.relay_info()

  @typedoc """
  List of WebRTC extensions to use.

  At this moment only VAD (RFC 6464) is supported.
  Enabling it will cause SFU sending `{:vad_notification, val, endpoint_id}` messages.
  """
  @type webrtc_extensions_t() :: [Membrane.WebRTC.Extension.t()]

  @typedoc """
  A map pointing from encoding names to lists of extensions that should be used for given encodings.
  Encoding "`:any`" indicates that extensions should be applied regardless of encoding.

  A sample usage would be to add silence discarder to OPUS tracks when VAD extension is enabled.
  It can greatly reduce CPU usage in rooms when there are a lot of people but only a few of
  them are actively speaking.
  """
  @type extensions_t() :: %{
          (encoding_name :: atom() | :any) => [Membrane.RTP.SessionBin.extension_t()]
        }

  @typedoc """
  SFU network configuration options.

  `dtls_pkey` and `dtls_cert` can be used e.g. when there are a lot of SFU instances
  and all of them need to use the same certificate and key.

  Example configuration can look like this:

  ```elixir
  network_options: [
    stun_servers: [
      %{server_addr: "stun.l.google.com", server_port: 19_302}
    ]
  ]
  """
  @type network_options_t() :: [
          stun_servers: [stun_server_t()],
          turn_servers: [turn_server_t()],
          integrated_turn_options: Membrane.ICE.Bin.integrated_turn_options_t(),
          dtls_pkey: binary(),
          dtls_cert: binary()
        ]

  @typedoc """
  RTC Engine configuration options.

  `id` is used by logger. If not provided it will be generated.
  """
  @type options_t() :: [
          id: String.t(),
          webrtc_extensions: webrtc_extensions_t(),
          extensions: extensions_t(),
          network_options: network_options_t()
        ]

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
    options = Keyword.put(options, :id, id)

    Membrane.Logger.info("Starting a new RTC Engine instance with id: #{id}")

    apply(Membrane.Pipeline, func, [
      __MODULE__,
      options,
      process_options
    ])
  end

  @spec get_registry_name() :: atom()
  def get_registry_name(), do: @registry_name

  @impl true
  def handle_init(options) do
    play(self())

    {{:ok, log_metadata: [sfu: options[:id]]},
     %{
       id: options[:id],
       peers: %{},
       endpoints: %{},
       options: options,
       integrated_turn_options:
         options[:network_options][:integrated_turn_options] || [use_integrated_turn: false],
       extensions: options[:extensions] || %{},
       webrtc_extensions: options[:webrtc_extensions] || [],
       waiting_for_linking: %{},
       filters: %{},
       subscriptions: %{}
     }}
  end

  @impl true
  def handle_other({:register, pid}, _ctx, state) do
    Registry.register(get_registry_name(), self(), pid)
    {:ok, state}
  end

  @impl true
  def handle_other({:unregister, pid}, _ctx, state) do
    Registry.unregister_match(get_registry_name(), self(), pid)
    {:ok, state}
  end

  @impl true
  def handle_other({:remove_peer, id}, ctx, state) do
    {actions, state} = remove_peer(id, ctx, state)
    {{:ok, actions}, state}
  end

  @impl true
  def handle_other({:add_endpoint, id, endpoint}, ctx, state) do
    {actions, state} =
      setup_peer(%{id: id, endpoint: endpoint, receive_media: true}, node(), ctx, state)

    {{:ok, actions}, state}
  end

  @impl true
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
    dispatch({:new_peer, peer_id, data.metadata})

    receive do
      {:accept_new_peer, ^peer_id, endpoint, peer_node} ->
        do_accept_new_peer(peer_id, peer_node, data, endpoint, ctx, state)

      {:accept_new_peer, ^peer_id, endpoint} ->
        do_accept_new_peer(peer_id, self(), data, endpoint, ctx, state)

      {:accept_new_peer, peer_id, _endpoint} ->
        Membrane.Logger.warn("Unknown peer id passed for acceptance: #{inspect(peer_id)}")
        {[], state}

      {:accept_new_peer, peer_id, _endpoint, peer_node} ->
        Membrane.Logger.warn(
          "Unknown peer id passed for acceptance: #{inspect(peer_id)} for node #{inspect(peer_node)}"
        )

        {[], state}

      {:deny_new_peer, peer_id} ->
        MediaEvent.create_peer_denied_event(peer_id)
        |> dispatch()

        {[], state}

      {:deny_new_peer, peer_id, data: data} ->
        MediaEvent.create_peer_denied_event(peer_id, data)
        |> dispatch()

        {[], state}
    end
  end

  defp handle_media_event(%{type: :sdp_offer} = event, peer_id, _ctx, state) do
    actions = [
      forward:
        {{:endpoint, peer_id},
         {:signal, {:sdp_offer, event.data.sdp_offer.sdp, event.data.mid_to_track_id}}}
    ]

    peer = get_in(state, [:peers, peer_id])
    peer = Map.put(peer, :track_id_to_track_metadata, event.data.track_id_to_track_metadata)
    state = put_in(state, [:peers, peer_id], peer)

    {actions, state}
  end

  defp handle_media_event(%{type: :candidate} = event, peer_id, _ctx, state) do
    actions = [forward: {{:endpoint, peer_id}, {:signal, {:candidate, event.data.candidate}}}]
    {actions, state}
  end

  defp handle_media_event(%{type: :leave}, peer_id, ctx, state) do
    {actions, state} = remove_peer(peer_id, ctx, state)
    {actions, state}
  end

  defp handle_media_event(%{type: :renegotiate_tracks}, peer_id, _ctx, state) do
    actions = [forward: {{:endpoint, peer_id}, {:signal, :renegotiate_tracks}}]
    {actions, state}
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

      MediaEvent.create_peer_updated_event(peer_id, updated_peer)
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
         peer_id,
         _ctx,
         state
       ) do
    peer = Map.get(state.peers, peer_id)

    if Map.get(peer.track_id_to_track_metadata, track_id) != track_metadata do
      peer = Map.update!(peer, :track_id_to_track_metadata, &%{&1 | track_id => track_metadata})
      state = put_in(state, [:peers, peer_id], peer)

      MediaEvent.create_track_updated_event(peer_id, track_id, track_metadata)
      |> dispatch()

      {[], state}
    else
      {[], state}
    end
  end

  @impl true
  def handle_notification(
        {:signal, {:sdp_answer, answer, mid_to_track_id}},
        {:endpoint, peer_id},
        _ctx,
        state
      ) do
    MediaEvent.create_signal_event(peer_id, {:signal, {:sdp_answer, answer, mid_to_track_id}})
    |> dispatch()

    {:ok, state}
  end

  @impl true
  def handle_notification({:signal, message}, {:endpoint, peer_id}, _ctx, state) do
    MediaEvent.create_signal_event(peer_id, {:signal, message})
    |> dispatch()

    {:ok, state}
  end

  # NOTE: When `payload_and_depayload_tracks?` options is set to false we may still want to depayload
  # some streams just in one place to e.g. dump them to HLS or perform any actions on depayloaded
  # media without adding payload/depaload elements to all EndpointBins (performing unnecessary work).
  #
  # To do that one just need to apply `depayloading_filter` after the tee element on which filter's the notification arrived.
  @impl true
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

    children = %{
      endpoint_tee => Membrane.Element.Tee.Master,
      fake => Membrane.Element.Fake.Sink.Buffers,
      filter => depayloading_filter,
      filter_tee => Membrane.Element.Tee.Master,
      filter_tee_fake => Membrane.Element.Fake.Sink.Buffers
    }

    # extensions = Map.get(state.extensions, encoding, []) ++ Map.get(state.extensions, :any, [])

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
  def handle_notification({:vad, val}, {:endpoint, endpoint_id}, _ctx, state) do
    dispatch({:vad_notification, val, endpoint_id})
    {:ok, state}
  end

  @impl true
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

    peer = get_in(state, [:peers, endpoint_id])

    MediaEvent.create_tracks_added_event(endpoint_id, peer.track_id_to_track_metadata)
    |> dispatch()

    {{:ok, tracks_msgs}, state}
  end

  @impl true
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
    |> dispatch()

    {{:ok, tracks_msgs}, state}
  end

  @impl true
  def handle_notification({:integrated_turn_servers, turns}, {:endpoint, peer_id}, _ctx, state) do
    turns = get_turn_configs(peer_id, turns)
    state = put_in(state, [:peers, peer_id, :integrated_turn_servers], turns)
    peer = state.peers[peer_id]
    enforce_turns? = state.integrated_turn_options[:use_integrated_turn] || false

    MediaEvent.create_peer_accepted_event(
      peer_id,
      Map.delete(state.peers, peer_id),
      turns,
      enforce_turns?
    )
    |> dispatch()

    MediaEvent.create_peer_joined_event(peer_id, peer)
    |> dispatch()

    {:ok, state}
  end

  @doc """
  This function create a notification, which should be send to all others endpoints in room.
  """
  @spec publish(msg :: any()) :: [Membrane.Notification.t()]
  def publish(msg), do: [notify: {:publish, msg}]

  @doc """
  This function create a notification, which is response for {:new_tracks, tracks} message. It should contains a list
  in form of pairs {track_id, track_format}, where track_id is id of track on which this endpoint want to subscribe and track_format
  is the format in which endpoint want to receive stream (e.g. raw, RTP). All possible format should be sent in field format in track in
  message {:new_tracks, tracks}.
  """
  @spec subscribe([{Track.id(), Track.format()}]) :: [Membrane.Notification.t()]
  def subscribe(tracks), do: [notify: {:subscribe, tracks}]

  @doc """
  This function allow to add endpoint outside of mechanism to :accept_new_peer
  """
  @spec add_endpoint(
          pid :: pid(),
          id :: String.t(),
          endpoint :: Membrane.ParentSpec.child_spec_t()
        ) :: none()
  def add_endpoint(pid, id, endpoint), do: send(pid, {:add_endpoint, id, endpoint})

  @doc """
  This function send message which start process of accepting new peer
  """
  @spec accept_peer(
          pid :: pid(),
          peer_id :: String.t(),
          endpoint :: Membrane.ParentSpec.child_spec_t(),
          peer_node :: node()
        ) ::
          none()
  def accept_peer(pid, peer_id, endpoint, peer_node \\ node()) do
    msg = {:accept_new_peer, peer_id, endpoint, peer_node}
    send(pid, msg)
  end

  @doc """
  This function send message responsible for register process inside pipeline.
  """
  @spec register(rtc_engine :: pid(), who :: pid()) :: none()
  def register(rtc_engine, who \\ self()), do: send(rtc_engine, {:register, who})

  defp link_inbound_track(track_id, tee, filter_tee, waiting_for_linking, ctx, state) do
    reduce_children(ctx, {[], waiting_for_linking}, fn
      {:endpoint, endpoint_id}, {new_links, waiting_for_linking} ->
        if MapSet.member?(waiting_for_linking[endpoint_id], track_id) do
          format = state.subscriptions[endpoint_id][track_id]

          format_specific_tee = if format == :raw, do: filter_tee, else: tee

          new_link =
            link(format_specific_tee)
            |> via_out(:copy)
            |> via_in(Pad.ref(:input, track_id))
            |> to({:endpoint, endpoint_id})

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
          new_link =
            link(format_specific_tee)
            |> via_out(:copy)
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
      for {_, pid} <- entries, do: send(pid, {self(), msg})
    end)
  end

  defp do_accept_new_peer(peer_id, peer_node, data, endpoint, ctx, state) do
    if Map.has_key?(state.peers, peer_id) do
      Membrane.Logger.warn("Peer with id: #{inspect(peer_id)} has already been added")
      {[], state}
    else
      peer =
        if Map.has_key?(data, :track_id_to_track_metadata) do
          data
        else
          Map.put(data, :track_id_to_track_metadata, %{})
        end
        |> Map.put(:id, peer_id)

      state = put_in(state, [:peers, peer_id], peer)
      do_setup_peer(Map.put(peer, :bin, bin), peer_node, state)
    end
  end

  defp do_setup_peer(config, peer_node, state) do
    inbound_tracks = []
    outbound_tracks = get_outbound_tracks(state.endpoints, config.receive_media)

    endpoint = Endpoint.new(config.id, inbound_tracks, %{receive_media: config.receive_media})

    endpoint_name = {:endpoint, config.id}

    children = %{
      endpoint_name => config.endpoint
    }

    action =
      if outbound_tracks != [],
        do: [forward: {endpoint_name, {:new_tracks, outbound_tracks}}],
        else: []

    state = put_in(state, [:waiting_for_linking, config.id], MapSet.new())

    spec = %ParentSpec{
      node: peer_node,
      children: children,
      crash_group: {config.id, :temporary}
    }

    state = put_in(state.endpoints[config.id], endpoint)

    {[spec: spec] ++ action, state}
  end

  defp get_turn_configs(name, turn_servers) do
    Enum.map(turn_servers, fn
      %{secret: secret} = turn_server ->
        {username, password} = TurnUtils.generate_credentials(name, secret)

        Map.delete(turn_server, :secret)
        |> Map.put(:username, username)
        |> Map.put(:password, password)

      other ->
        other
    end)
  end

  defp get_outbound_tracks(endpoints, true),
    do: Enum.flat_map(endpoints, fn {_id, endpoint} -> Endpoint.get_tracks(endpoint) end)

  defp get_outbound_tracks(_endpoints, false), do: []

  defp remove_peer(peer_id, ctx, state) do
    case do_remove_peer(peer_id, ctx, state) do
      {:absent, [], state} ->
        Membrane.Logger.info("Peer #{inspect(peer_id)} already removed")
        {[], state}

      {:present, actions, state} ->
        {_waiting, state} = pop_in(state, [:waiting_for_linking, peer_id])

        MediaEvent.create_peer_left_event(peer_id)
        |> dispatch()

        {actions, state}
    end
  end

  defp do_remove_peer(peer_id, ctx, state) do
    if Map.has_key?(state.endpoints, peer_id) do
      {endpoint, state} = pop_in(state, [:endpoints, peer_id])
      {_peer, state} = pop_in(state, [:peers, peer_id])
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
          children =
            Endpoint.get_tracks(endpoint)
            |> Enum.map(fn track -> track.id end)
            |> Enum.flat_map(&[tee: {peer_id, &1}, fake: {peer_id, &1}])
            |> Enum.filter(&Map.has_key?(ctx.children, &1))

          children = [endpoint: peer_id] ++ children
          [remove_child: children]
        end

      {:present, tracks_msgs ++ actions, state}
    else
      {:absent, [], state}
    end
  end

  defp do_publish(_endpoints, _ctx, [] = _tracks, _endpoint_bin), do: []

  defp do_publish(endpoints, ctx, msg, endpoint_bin_name) do
    flat_map_children(ctx, fn
      {:endpoint, endpoint_id} = other_endpoint_bin ->
        endpoint = Map.get(endpoints, endpoint_id)

        if other_endpoint_bin != endpoint_bin_name and not is_nil(endpoint) and
             endpoint.ctx.receive_media do
          [forward: {other_endpoint_bin, msg}]
        else
          []
        end

      _child ->
        []
    end)
  end
end
