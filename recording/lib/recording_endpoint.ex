defmodule Membrane.RTC.Engine.Endpoint.Recording do
  @moduledoc """
  An Endpoint responsible for saving incoming tracks to pointed storages.
  """

  use Membrane.Bin

  require Membrane.Logger

  alias Membrane.RTC.Engine
  alias Membrane.RTC.Engine.Endpoint.Recording.Reporter
  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver

  @track_children [:track_receiver, :serializer, :tee]

  def_input_pad :input,
    accepted_format: Membrane.RTP,
    availability: :on_request

  def_options owner: [
                spec: pid(),
                description: """
                Pid of parent all notifications will be send to.
                These notifications are:
                """
              ],
              rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              stores: [
                spec: [module()],
                description: "A list of stores that the recorded streams will be uploaded to"
              ],
              output_dir: [
                spec: Path.t(),
                default: "output",
                description: """
                Directory that contains output files. For S3, this is the object's path prefix.
                """
              ],
              recording_id: [
                spec: String.t(),
                description: """
                Recording id that will be saved along with raport
                """
              ]

  @impl true
  def handle_init(ctx, options) do
    {:ok, reporter} = Reporter.start(options.recording_id)

    Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
      save_reports(reporter, options.stores, options.output_dir)
    end)

    state =
      options
      |> Map.from_struct()
      |> Map.merge(%{
        tracks: %{},
        reporter: reporter,
        start_timestamp: nil
      })

    {[notify_parent: :ready], state}
  end

  @impl true
  def handle_parent_notification({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    Enum.each(tracks, fn track ->
      case Engine.subscribe(state.rtc_engine, endpoint_id, track.id) do
        :ok ->
          :ok

        {:error, reason} ->
          reason = inspect(reason)

          Membrane.Logger.error(%{
            error: reason,
            message: "Couldn't subscribe to track",
            track: track
          })

          raise "Subscription to track #{track.id} failed with reason: `#{reason}`"
      end
    end)

    tracks = Map.new(tracks, &{&1.id, &1})
    state = Map.update!(state, :tracks, &Map.merge(&1, tracks))

    {[], state}
  end

  def handle_parent_notification(_notification, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    {offset, state} = calculate_offset(state)

    track = Map.get(state.tracks, track_id)
    spec = get_track_spec(track, pad) ++ attach_sinks(track, state)

    Reporter.add_track(state.reporter, track, filename(track), offset)

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
      |> Enum.map(&{:sink, track_id, &1})
      |> Enum.filter(&Map.has_key?(ctx.children, &1))

    track_children = track_elements ++ track_sinks
    {_track, state} = pop_in(state, [:tracks, track_id])
    {[remove_children: track_children], state}
  end

  defp get_track_spec(track, pad) do
    [
      bin_input(pad)
      |> child({:track_receiver, track.id}, %TrackReceiver{
        track: track,
        initial_target_variant: :high
      })
      |> child({:serializer, track.id}, Membrane.Stream.Serializer)
      |> child({:tee, track.id}, Membrane.Tee.Parallel)
    ]
  end

  defp save_reports(reporter, stores, output_dir) do
    Reporter.get_report(reporter)

    report_json =
      reporter
      |> Reporter.get_report()
      |> Jason.encode!()

    Enum.each(stores, fn module ->
      unless module.save_object(report_json, output_dir, "report.json") == :ok do
        Membrane.Logger.error(%{
          message: "Failed to save report",
          object: "report.json",
          storege: module
        })
      end
    end)

    Reporter.stop(reporter)
  end

  defp attach_sinks(track, state) do
    sink_opts = %{
      track: track,
      output_dir: state.output_dir,
      filename: filename(track)
    }

    Enum.map(state.stores, fn module ->
      get_child({:tee, track.id}) |> child({:sink, track.id, module}, module.get_sink(sink_opts))
    end)
  end

  defp calculate_offset(%{start_timestamp: nil} = state),
    do: {0, %{state | start_timestamp: System.monotonic_time()}}

  defp calculate_offset(state), do: {System.monotonic_time() - state.start_timestamp, state}

  defp filename(track), do: "#{track.type}_#{track.id}.msr"
end
