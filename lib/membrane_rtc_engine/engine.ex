defmodule Membrane.RTC.Engine do
  @moduledoc """
  SFU engine implementation.

  One SFU instance is responsible for managing one room in which
  all tracks of one peer are forwarded to all other peers.

  The SFU engine works by sending and receiving messages.
  All messages are described below.
  To receive SFU messages you have to register your process so that SFU will
  know where to send its messages.

  ## Registering for messages

  Registration can be done by sending the message `{register, pid}` to the SFU instance, e.g.

  ```elixir
  send(sfu_pid, {:register, self()})
  ```

  This will register your process to receive SFU messages.
  If your process implements `GenServer` behaviour then all messages will be handled
  by `c:GenServer.handle_info/2`, e.g.

  ```elixir
  @impl true
  def handle_info({_sfu_engine, {:sfu_media_event, :broadcast, event}}, state) do
    for {_peer_id, pid} <- state.peer_channels, do: send(pid, {:media_event, event})
    {:noreply, state}
  end
  ```

  You can register multiple processes to receive messages from an SFU instance.
  In such a case each message will be sent to each registered process.

  ## Media Events

  The SFU engine needs to communicate with Membrane client libraries.
  This communication is done via `Media Event` messages.
  Media Events are blackbox messages that carry data important for the
  SFU engine and client libraries, but not for the user. Example Media Events are
  SDP offers, ICE candidates, and information about new peers.

  An application is obligated to transport Media Events from an SFU instance to
  its client library, and vice versa.

  When an SFU needs to send a message to a specific client, registered processes will
  receive `{:sfu_media_event, to, event}`, where `to` specifies the message destination.
  This can be either `:broadcast`, when the event should be sent to all peers, or `peer_id`
  when the messages should be sent to specified peer. The `event` is encoded in binary format,
  so it is ready to send without modification.

  Feeding an SFU instance with Media Events from a client library can be done by sending the
  message `{:media_event, from, event}`. Assuming the user process is a GenServer, the
  Media Event can be received by `c:GenServer.handle_info/2` and conveyed to the SFU engine in
  the following way:

  ```elixir
  @impl true
  def handle_info({:media_event, _from, _event} = msg, state) do
    send(state.sfu_engine, msg)
    {:noreply, state}
  end
  ```

  What is important, Membrane SFU doesn't impose usage of any specific transport layer.
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

  Each message the SFU sends is a two-element tuple `{sfu_pid, msg}` where
  `sfu_pid` is the pid of the SFU instance that sent message, and `msg` can be any data.

  Notice that thanks to presence of `sfu_pid` you can create multiple SFU instances.

  Example SFU message:

  ```elixir
  {_sfu_pid, {:vad_notification, val, peer_id}}
  ```

  #### SFU sends following messages

  * `{:sfu_media_event, to, event}` - a Media Event that should be transported to the client
  library. When `from` is `:broadcast`, the Media Event should be sent to all peers. When
  `from` is a `peer_id`, the Media Event should be sent to that specified peer.
  * `{:vad_notification, val, peer_id}` - sent when peer with id `peer_id` is speaking.
  `VAD` stands for `Voice Activity Detection`. When `val` is `true` marks start of speech
  whereas `false` marks end of speech.
  * `{:new_peer, peer_id, metadata, track_metadata}` - sent when a new peer tries to join
  to an SFU instance. `metadata` is any data passed by the client library while joining.
  `track_metadata` is a map where key is track id and value is its metadata defined in client
  library while adding a new track.
  * `{:peer_left, peer_id}` - sent when the peer with `peer_id` leaves an SFU instance

  #### SFU receives following messages

  * `{:register, pid}` - register given `pid` for receiving SFU messages
  * `{:unregister, pid}` - unregister given `pid` from receiving SFU messages
  * `{:media_event, from, event}` - feed Media Event to SFU. `from` is id of peer
  that this Media event comes from.
  * `{:accept_new_peer, peer_id}` - accepts peer with id `peer_id`
  * `{:deny_new_peer, peer_id}` - denies peer with id `peer_id`
  * `{:remove_peer, peer_id}` - removes peer with id `peer_id`

  ## Peer id

  Peer ids must be assigned by application code. This is not done by the SFU engine or its client library.
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

  alias Membrane.WebRTC.{Endpoint, EndpointBin, Track}
  alias Membrane.RTC.Engine.MediaEvent

  require Membrane.Logger

  @registry_name Membrane.RTC.Engine.Registry.Dispatcher

  @type stun_server_t() :: ExLibnice.stun_server()
  @type turn_server_t() :: ExLibnice.relay_info()

  @typedoc """
  List of RTP extensions to use.

  At this moment only `vad` extension is supported.
  Enabling it will cause SFU sending `{:vad_notification, val, endpoint_id}` messages.
  """
  @type extension_options_t() :: [
          vad: boolean()
        ]

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
          dtls_pkey: binary(),
          dtls_cert: binary()
        ]

  @typedoc """
  SFU configuration options.

  `id` is used by logger. If not provided it will be generated.
  """
  @type options_t() :: [
          id: String.t(),
          extension_options: extension_options_t(),
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

    Membrane.Logger.info("Starting a new SFU instance with id: #{id}")

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
       incoming_peers: %{},
       endpoints: %{},
       track_id_to_peers: %{},
       options: options
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
  def handle_other({:media_event, from, data}, ctx, state) do
    case MediaEvent.deserialize(data) do
      {:ok, event} ->
        {actions, state} = handle_media_event(event, from, ctx, state)
        {{:ok, actions}, state}

      {:error, :invalid_media_event} ->
        Membrane.Logger.warn("Invalid media event #{inspect(data)}")
        {:ok, state}
    end
  end

  defp handle_media_event(%{type: :join, data: data}, peer_id, ctx, state) do
    dispatch({:new_peer, peer_id, data.metadata, data.tracks_metadata})

    receive do
      {:accept_new_peer, ^peer_id} ->
        if Map.has_key?(state.peers, peer_id) do
          Membrane.Logger.warn("Peer with id: #{inspect(peer_id)} has already been added")
          {[], state}
        else
          peer = Map.put(data, :id, peer_id)
          state = put_in(state, [:incoming_peers, peer_id], peer)
          {actions, state} = setup_peer(peer, ctx, state)

          MediaEvent.create_peer_accepted_event(peer_id, Map.delete(state.peers, peer_id))
          |> dispatch()

          {actions, state}
        end

      {:accept_new_peer, _other_peer_id} ->
        Membrane.Logger.warn("Unknown peer id passed for acceptance: #{inspect(peer_id)}")
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
      forward: {{:endpoint, peer_id}, {:signal, {:sdp_offer, event.data.sdp_offer.sdp}}}
    ]

    state =
      if Map.has_key?(state.incoming_peers, peer_id) do
        {peer, state} = pop_in(state, [:incoming_peers, peer_id])
        peer = Map.delete(peer, :tracks_metadata)
        peer = Map.put(peer, :mid_to_track_metadata, event.data.mid_to_track_metadata)
        state = put_in(state, [:peers, peer_id], peer)

        MediaEvent.create_peer_joined_event(
          peer_id,
          state.peers[peer_id].metadata,
          event.data.mid_to_track_metadata
        )
        |> dispatch()

        state
      else
        state
      end

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

  defp get_mids_to_tracks_and_peer(mid_to_track, peer_id, state) do
    track_to_peer_id =
      for {_id, endpoint} <- state.endpoints,
          track <- Endpoint.get_tracks(endpoint),
          reduce: %{} do
        acc -> Map.put(acc, track.id, endpoint.id)
      end

    for {mid, track} <- mid_to_track, reduce: %{} do
      acc -> Map.put(acc, mid, %{track: track, id: Map.get(track_to_peer_id, track, peer_id)})
    end
  end

  @impl true
  def handle_notification({:mid_to_track, mid_to_track}, {:endpoint, peer_id}, _ctx, state) do
    mid_to_track = get_mids_to_tracks_and_peer(mid_to_track, peer_id, state)

    MediaEvent.create_peer_mids_update_event(
      peer_id,
      mid_to_track
    )
    |> dispatch()

    {:ok, state}
  end

  @impl true
  def handle_notification({:signal, message}, {:endpoint, peer_id}, _ctx, state) do
    MediaEvent.create_signal_event(peer_id, {:signal, message})
    |> dispatch()

    {:ok, state}
  end

  @impl true
  def handle_notification({:new_track, track_id, encoding}, endpoint_bin_name, ctx, state) do
    Membrane.Logger.info(
      "New incoming #{encoding} track #{track_id} from #{inspect(endpoint_bin_name)}"
    )

    {:endpoint, endpoint_id} = endpoint_bin_name

    tee = {:tee, {endpoint_id, track_id}}
    fake = {:fake, {endpoint_id, track_id}}

    children = %{
      tee => Membrane.Element.Tee.Master,
      fake => Membrane.Element.Fake.Sink.Buffers
    }

    extensions = setup_extensions(encoding, state[:options][:extension_options])

    peers = Map.get(state.track_id_to_peers, track_id, [])

    link_to_fake =
      link(endpoint_bin_name)
      |> via_out(Pad.ref(:output, track_id), options: [extensions: extensions])
      |> to(tee)
      |> via_out(:master)
      |> to(fake)

    links_and_endpoints =
      flat_map_children(ctx, fn
        {:endpoint, other_endpoint_id} = other_endpoint_name ->
          if other_endpoint_id in peers and
               state.endpoints[other_endpoint_id].ctx.receive_media do
            [
              {link(tee)
               |> via_out(:copy)
               |> via_in(Pad.ref(:input, track_id), options: [encoding: encoding])
               |> to(other_endpoint_name), other_endpoint_name}
            ]
          else
            []
          end

        _child ->
          []
      end)

    links = [link_to_fake | Enum.map(links_and_endpoints, fn {link, _endpoint} -> link end)]
    endpoints = Enum.map(links_and_endpoints, fn {_link, endpoint} -> endpoint end)

    actions = Enum.flat_map(endpoints, &[forward: {&1, {:linked_tracks, [track_id]}}])

    spec = %ParentSpec{children: children, links: links, crash_group: {endpoint_id, :temporary}}

    state =
      update_in(
        state,
        [:endpoints, endpoint_id],
        &Endpoint.update_track_encoding(&1, track_id, encoding)
      )

    {{:ok, actions ++ [spec: spec]}, state}
  end

  @impl true
  def handle_notification({:vad, val}, {:endpoint, endpoint_id}, _ctx, state) do
    dispatch({:vad_notification, val, endpoint_id})
    {:ok, state}
  end

  @impl true
  def handle_notification(
        {:negotiation_done, tracks},
        {:endpoint, endpoint_id} = endpoint_bin,
        ctx,
        state
      ) do
    links_and_tracks =
      Enum.flat_map(
        tracks,
        &link_track_if_tee_exist(&1, endpoint_id, ctx, state.track_id_to_peers)
      )

    links = Enum.map(links_and_tracks, fn {link, _track} -> link end)
    linked_tracks = Enum.map(links_and_tracks, fn {_link, track} -> track end)

    tracks_waiting_for_linking =
      Enum.map(tracks, fn {track_id, _encoding} -> track_id end)
      |> Enum.filter(&(&1 not in linked_tracks))

    track_id_to_peers =
      Map.new(state.track_id_to_peers, fn {track_id, peers} ->
        peers =
          if track_id in tracks_waiting_for_linking,
            do: Enum.uniq([endpoint_id | peers]),
            else: peers

        {track_id, peers}
      end)

    actions = [forward: {endpoint_bin, {:linked_tracks, linked_tracks}}]

    state = %{state | track_id_to_peers: track_id_to_peers}

    {{:ok, [spec: %ParentSpec{links: links}] ++ actions}, state}
  end

  @impl true
  def handle_notification({:new_tracks, tracks}, {:endpoint, endpoint_id}, ctx, state) do
    id_to_track = Map.new(tracks, &{&1.id, &1})

    endpoint = state.endpoints[endpoint_id]
    endpoint = %Endpoint{endpoint | inbound_tracks: id_to_track}
    state = put_in(state.endpoints[endpoint_id], endpoint)

    track_id_to_peers =
      Enum.reduce(tracks, state.track_id_to_peers, fn
        track, acc -> Map.put(acc, track.id, [])
      end)

    state = %{state | track_id_to_peers: track_id_to_peers}

    tracks_msgs = update_track_messages(ctx, tracks, {:endpoint, endpoint_id})
    {{:ok, tracks_msgs}, state}
  end

  defp link_track_if_tee_exist({track_id, encoding}, endpoint_id, ctx, track_id_to_peers) do
    peers = Map.get(track_id_to_peers, track_id, [])

    tee =
      ctx.children
      |> Map.keys()
      |> Enum.map(fn
        {:tee, {_endpoint_id, ^track_id}} = tee -> tee
        _child -> nil
      end)
      |> Enum.filter(&(&1 != nil))

    if tee != [] and endpoint_id not in peers do
      Enum.map(
        tee,
        &{link(&1)
         |> via_out(:copy)
         |> via_in(Pad.ref(:input, track_id), options: [encoding: encoding])
         |> to({:endpoint, endpoint_id}), track_id}
      )
    else
      []
    end
  end

  defp dispatch(msg) do
    Registry.dispatch(get_registry_name(), self(), fn entries ->
      for {_, pid} <- entries, do: send(pid, {self(), msg})
    end)
  end

  defp setup_peer(config, _ctx, state) do
    inbound_tracks = []
    outbound_tracks = get_outbound_tracks(state.endpoints, config.receive_media)

    # TODO `type` field should probably be deleted from Endpoint struct
    endpoint =
      Endpoint.new(config.id, :participant, inbound_tracks, %{receive_media: config.receive_media})

    handshake_opts =
      if state.options[:network_options][:dtls_pkey] &&
           state.options[:network_options][:dtls_cert] do
        [
          client_mode: false,
          dtls_srtp: true,
          pkey: state.options[:network_options][:dtls_pkey],
          cert: state.options[:network_options][:dtls_cert]
        ]
      else
        [
          client_mode: false,
          dtls_srtp: true
        ]
      end

    children = %{
      {:endpoint, config.id} => %EndpointBin{
        outbound_tracks: outbound_tracks,
        inbound_tracks: inbound_tracks,
        stun_servers: state.options[:network_options][:stun_servers] || [],
        turn_servers: state.options[:network_options][:turn_servers] || [],
        handshake_opts: handshake_opts,
        log_metadata: [peer_id: config.id]
      }
    }

    spec = %ParentSpec{children: children, crash_group: {config.id, :temporary}}

    state = put_in(state.endpoints[config.id], endpoint)

    {[spec: spec], state}
  end

  defp get_outbound_tracks(endpoints, true),
    do: Enum.flat_map(endpoints, fn {_id, endpoint} -> Endpoint.get_tracks(endpoint) end)

  defp get_outbound_tracks(_endpoints, false), do: []

  defp setup_extensions(encoding, extension_options) do
    if encoding == :OPUS and extension_options[:vad], do: [{:vad, Membrane.RTP.VAD}], else: []
  end

  defp remove_peer(peer_id, ctx, state) do
    case do_remove_peer(peer_id, ctx, state) do
      {:absent, [], state} ->
        Membrane.Logger.info("Peer #{inspect(peer_id)} already removed")
        {[], state}

      {:present, actions, state} ->
        MediaEvent.create_peer_left_event(peer_id)
        |> dispatch()

        {actions, state}
    end
  end

  defp do_remove_peer(peer_id, ctx, state) do
    if Map.has_key?(state.endpoints, peer_id) do
      {endpoint, state} = pop_in(state, [:endpoints, peer_id])
      {_peer, state} = pop_in(state, [:peers, peer_id])
      tracks = Enum.map(Endpoint.get_tracks(endpoint), &%Track{&1 | enabled?: false})

      tracks_msgs = update_track_messages(ctx, tracks, {:endpoint, peer_id})

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

  defp update_track_messages(_ctx, [] = _tracks, _endpoint_bin), do: []

  defp update_track_messages(ctx, tracks, endpoint_bin_name) do
    flat_map_children(ctx, fn
      {:endpoint, _endpoint_id} = other_endpoint_bin
      when other_endpoint_bin != endpoint_bin_name ->
        [forward: {other_endpoint_bin, {:add_tracks, tracks}}]

      _child ->
        []
    end)
  end

  defp flat_map_children(ctx, fun) do
    ctx.children |> Map.keys() |> Enum.flat_map(fun)
  end
end
