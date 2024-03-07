defmodule Membrane.RTC.Engine.Endpoint.Recording.Reporter do
  @moduledoc """
  Module responsible for creating report with information needed to decode recorded streams.
  """

  use GenServer

  alias Membrane.RTC.Engine.Track

  @track_reports_keys [:type, :encoding, :start_timestamp, :end_timestamp, :clock_rate, :metadata]

  @type filename :: String.t()
  @type track_report :: %{
          type: Track.t(),
          encoding: Track.encoding(),
          timestamp: pos_integer(),
          clock_rate: Membrane.RTP.clock_rate_t(),
          metadata: any()
        }
  @type report :: %{
          recording_id: String.t(),
          tracks: %{filename() => track_report()}
        }

  @opaque state :: %{tracks: [], recording_id: String.t()}

  @spec start(String.t()) :: {:ok, pid()} | {:error, term()}
  def start(recording_id) do
    GenServer.start(__MODULE__, recording_id)
  end

  @spec add_track(pid(), Track.t(), filename(), pos_integer()) :: :ok
  def add_track(reporter, track, filename, timestamp) do
    track = Map.put(track, :start_timestamp, timestamp)
    GenServer.cast(reporter, {:add_track, track, filename})
  end

  @spec end_track(pid(), Track.t(), pos_integer()) :: :ok
  def end_track(reporter, track, end_timestamp) do
    GenServer.cast(reporter, {:end_track, track, end_timestamp})
  end

  @spec get_report(pid()) :: report()
  def get_report(reporter) do
    GenServer.call(reporter, :get_report)
  end

  @spec get_timestamp() :: number()
  def get_timestamp() do
    System.monotonic_time()
  end

  @spec stop(pid()) :: :ok
  def stop(reporter) do
    GenServer.stop(reporter)
  end

  @impl true
  def init(recording_id) do
    {:ok, %{recording_id: recording_id, tracks: %{}}}
  end

  @impl true
  def handle_cast({:add_track, track, filename}, state) do
    state = put_in(state[:tracks][track.id], {filename, track})
    {:noreply, state}
  end

  @impl true
  def handle_cast({:end_track, track, end_timestamp}, state) do
    state =
      update_in(state[:tracks][track.id], fn {filename, track} ->
        {filename, Map.put(track, :end_timestamp, end_timestamp)}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_call(:get_report, _from, state) do
    {_filename, %{start_timestamp: start_timestamp}} =
      state.tracks
      |> Map.values()
      |> Enum.min_by(
        fn {_filename, track} -> track.start_timestamp end,
        &<=/2,
        fn -> {nil, 0} end
      )

    end_timestamp = get_timestamp() - start_timestamp

    tracks_report =
      state.tracks
      |> Map.values()
      |> Map.new(fn {filename, track} ->
        track =
          track
          |> Map.update!(:start_timestamp, &(&1 - start_timestamp))
          |> Map.update(:end_timestamp, end_timestamp, &(&1 - start_timestamp))

        {filename, Map.take(track, @track_reports_keys)}
      end)

    report = %{recording_id: state.recording_id, tracks: tracks_report}

    {:reply, report, state}
  end
end
