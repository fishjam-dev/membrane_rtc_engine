defmodule Membrane.RTC.Engine.Endpoint.Recording do
  @moduledoc """
  An Endpoint responsible for saving incoming tracks to pointed storages.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.Recording.{EdgeTimestampSaver, Guard, Reporter, Storage}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver
  alias Membrane.RTC.Engine.Subscriber

  @type storage_opts :: any()

  @doc """
  Implementation of `Membrane.RTC.Engine.Endpoint.Recording.Storage` behaviour
  """
  @type storage :: module()

  @track_children [:track_receiver, :serializer, :tee, :edge_timestamp_saver]

  def_input_pad :input,
    accepted_format: Membrane.RTP,
    availability: :on_request

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              stores: [
                spec: [{storage(), storage_opts()}],
                description: """
                A list of stores that the recorded streams will be uploaded to.
                Should implement `Membrane.RTC.Engine.Endpoint.Recording.Storage` behaviour.
                """
              ],
              recording_id: [
                spec: String.t(),
                description: """
                Recording id that will be saved along with report
                """
              ],
              subscribe_mode: [
                spec: :auto | :manual,
                default: :auto,
                description: """
                Whether tracks should be subscribed automatically when they're ready.
                If set to `:manual`, Recording endpoint will subscribe only to tracks from endpoints sent using message:
                `{:subscribe, endpoints}`
                """
              ]

  @doc """
  Subscribe Recording endpoint to tracks from endpoints.

  It is only valid to use when Recording has `subscribe_mode` set to :manual.
  """
  @spec subscribe(engine :: pid(), endpoint_id :: any(), endpoints :: [any()]) :: :ok
  def subscribe(engine, endpoint_id, endpoints) do
    Engine.message_endpoint(engine, endpoint_id, {:subscribe, endpoints})
  end

  @impl true
  def handle_init(_context, options) when options.subscribe_mode not in [:auto, :manual] do
    raise("""
    Cannot initialize Recording endpoint.
    Invalid value for `:subscribe_mode`: #{options.subscribe_mode}.
    Please set `:subscribe_mode` to either `:auto` or `:manual`.
    """)
  end

  @impl true
  def handle_init(ctx, options) do
    {:endpoint, endpoint_id} = ctx.name

    subscriber = %Subscriber{
      subscribe_mode: options.subscribe_mode,
      endpoint_id: endpoint_id,
      rtc_engine: options.rtc_engine
    }

    {:ok, reporter} = Reporter.start(options.recording_id)

    Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
      Task.Supervisor.start_child(
        __MODULE__.TaskSupervisor,
        fn ->
          stores = Guard.close_recording(options.stores, options.recording_id)
          save_reports(reporter, stores, options.recording_id)
        end,
        restart: :transient,
        shutdown: :infinity
      )
    end)

    state =
      options
      |> Map.from_struct()
      |> Map.merge(%{
        start_timestamp: nil,
        reporter: reporter,
        subscriber: subscriber
      })

    {[notify_parent: :ready], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, _ctx, state) do
    subscriber = Subscriber.handle_new_tracks(tracks, state.subscriber)

    {[], %{state | subscriber: subscriber}}
  end

  @impl true
  def handle_parent_notification(
        {:subscribe, endpoints},
        _ctx,
        state
      ) do
    subscriber = Subscriber.add_endpoints(endpoints, state.subscriber)
    {[], %{state | subscriber: subscriber}}
  end

  def handle_parent_notification(_notification, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    {offset, state} = calculate_offset(state)
    filename = generate_filename()

    track = Subscriber.get_track(state.subscriber, track_id)
    spec = spawn_track(track, filename, state) ++ link_track(track, pad, state)

    Reporter.add_track(state.reporter, track, filename, offset)

    {[spec: spec], state}
  end

  @impl true
  def handle_pad_removed(Pad.ref(:input, track_id), ctx, state) do
    track_elements =
      @track_children
      |> Enum.map(&{&1, track_id})
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    track_sinks =
      state.stores
      |> Enum.map(fn {storage, _opts} -> {:sink, track_id, storage} end)
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    track_children = track_elements ++ track_sinks

    new_subscriber = Subscriber.remove_track(state.subscriber, track_id)

    state = %{state | subscriber: new_subscriber}

    if Subscriber.get_tracks(state.subscriber) == %{} do
      Membrane.Logger.info("All tracks were removed. Stop recording.")
      {[remove_children: track_children, notify_parent: :finished], state}
    else
      {[remove_children: track_children], state}
    end
  end

  @impl true
  def handle_crash_group_down({module, track_id}, _ctx, state) do
    error = "Sink #{module} of track #{track_id} crashed"

    unless has_file_storage?(state.stores), do: raise(error)

    Membrane.Logger.warning(error)

    {[], state}
  end

  defp spawn_track(track, filename, state) do
    [
      [
        child({:track_receiver, track.id}, %TrackReceiver{
          track: track,
          initial_target_variant: :high
        })
        |> child({:edge_timestamp_saver, track.id}, %EdgeTimestampSaver{
          reporter: state.reporter
        })
        |> child({:serializer, track.id}, Membrane.Stream.Serializer)
        |> child({:tee, track.id}, Membrane.Tee.Parallel)
      ] ++ spawn_sinks(track, filename, state)
    ]
  end

  defp link_track(track, pad, state) do
    [
      bin_input(pad)
      |> get_child({:track_receiver, track.id})
    ] ++ link_sinks(track, state)
  end

  defp spawn_sinks(track, filename, state) do
    config = %{
      track: track,
      recording_id: state.recording_id,
      filename: filename
    }

    Enum.map(state.stores, fn {storage, opts} ->
      child = child({:sink, track.id, storage}, storage.get_sink(config, opts))

      case storage do
        Storage.File -> child
        module -> {child, group: {module, track.id}, crash_group_mode: :temporary}
      end
    end)
  end

  defp link_sinks(track, state) do
    Enum.map(state.stores, fn {storage, _opts} ->
      get_child({:tee, track.id})
      |> get_child({:sink, track.id, storage})
    end)
  end

  defp save_reports(reporter, stores, recording_id) do
    case Reporter.get_report(reporter) do
      %{tracks: tracks, recording_id: _recoridng_id} when tracks == %{} ->
        Membrane.Logger.warning("No tracks in report; not saving it to storage.")

      report ->
        report_json = Jason.encode!(report)

        Enum.each(stores, fn {storage, opts} ->
          config = %{
            object: report_json,
            recording_id: recording_id,
            filename: "report.json"
          }

          unless storage.save_object(config, opts) == :ok do
            Membrane.Logger.error(%{
              message: "Failed to save report",
              object: "report.json",
              storage: storage
            })
          end
        end)
    end

    Reporter.stop(reporter)
  end

  defp calculate_offset(%{start_timestamp: nil} = state),
    do: {0, %{state | start_timestamp: System.monotonic_time()}}

  defp calculate_offset(state), do: {System.monotonic_time() - state.start_timestamp, state}
  defp generate_filename(), do: "#{UUID.uuid4()}.msr"

  defp has_file_storage?(stores),
    do: Enum.any?(stores, fn {storage, _opts} -> storage == Storage.File end)
end
