defmodule Membrane.RTC.Engine.Endpoint.Recording.Reporter do
  @moduledoc """
  Module responsible for creating report with information needed to decode recorded streams.
  """

  use GenServer

  alias Membrane.RTC.Engine.Track

  @track_reports_keys [:type, :encoding, :offset, :clock_rate, :metadata]

  @type filename :: String.t()
  @type track_report :: %{
          type: Track.t(),
          encoding: Track.encoding(),
          offset: pos_integer(),
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
  def add_track(reporter, track, filename, offset) do
    track = Map.put(track, :offset, offset)
    GenServer.cast(reporter, {:add_track, track, filename})
  end

  @spec get_report(pid()) :: report()
  def get_report(reporter) do
    GenServer.call(reporter, :get_report)
  end

  @spec stop(pid()) :: :ok
  def stop(reporter) do
    GenServer.stop(reporter)
  end

  @impl true
  def init(recording_id) do
    {:ok, %{recording_id: recording_id, tracks: []}}
  end

  @impl true
  def handle_cast({:add_track, track, filename}, state) do
    state = Map.update!(state, :tracks, fn tracks -> [{filename, track} | tracks] end)
    {:noreply, state}
  end

  @impl true
  def handle_call(:get_report, _from, state) do
    tracks_report =
      state.tracks
      |> Enum.map(fn {filename, track} -> {filename, Map.take(track, @track_reports_keys)} end)
      |> Map.new()

    report = %{recording_id: state.recording_id, tracks: tracks_report}

    {:reply, report, state}
  end
end
