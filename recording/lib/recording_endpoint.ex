defmodule Membrane.RTC.Engine.Endpoint.Recording do
  @moduledoc """
  An Endpoint responsible for saving incoming tracks to pointed storages.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.Recording.{EdgeTimestampSaver, Guard, Reporter, Storage}
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver

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
              ]

  @impl true
  def handle_init(ctx, options) do
    {:ok, reporter} = Reporter.start(options.recording_id)

    Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
      stores = Guard.close_recording(options.stores, options.recording_id)
      save_reports(reporter, stores, options.recording_id)
    end)

    state =
      options
      |> Map.from_struct()
      |> Map.merge(%{
        tracks: %{},
        start_timestamp: nil,
        reporter: reporter
      })

    {[notify_parent: :ready], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    {valid_tracks, _invalid_tracks} =
      Enum.split_with(tracks, fn track ->
        case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
          :ok ->
            true

          :ignored ->
            false
        end
      end)

    tracks = Map.new(valid_tracks, &{&1.id, &1})
    state = Map.update!(state, :tracks, &Map.merge(&1, tracks))

    {[], state}
  end

  def handle_parent_notification(_notification, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    {offset, state} = calculate_offset(state)
    filename = generate_filename()

    track = Map.get(state.tracks, track_id)
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
    {_track, state} = pop_in(state, [:tracks, track_id])

    if state.tracks == %{} do
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
