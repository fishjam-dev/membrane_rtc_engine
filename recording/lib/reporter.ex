defmodule Membrane.RTC.Engine.Endpoint.Recording.Reporter do
  @moduledoc """
  Module responsible for creating report with information needed to decode recorded streams.
  """

  use GenServer

  alias Membrane.RTC.Engine.Track
  alias Membrane.RTCP.SenderReportPacket

  @track_reports_keys [
    :type,
    :encoding,
    :offset,
    :start_timestamp,
    :end_timestamp,
    :clock_rate,
    :metadata,
    :origin
  ]

  @type filename :: String.t()
  @type track_report :: %{
          type: Track.t(),
          encoding: Track.encoding(),
          offset: pos_integer(),
          start_timestamp: pos_integer(),
          start_timestamp_wallclock: pos_integer(),
          end_timestamp: pos_integer(),
          clock_rate: Membrane.RTP.clock_rate_t(),
          metadata: any(),
          origin: String.t()
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
    track = Map.put(track, :offset, timestamp)

    GenServer.cast(reporter, {:add_track, track, filename})
  end

  @spec start_timestamp(pid(), Track.id(), pos_integer()) :: :ok
  def start_timestamp(reporter, track_id, start_timestamp) do
    GenServer.cast(reporter, {:start_timestamp, track_id, start_timestamp})
  end

  @spec end_timestamp(pid(), Track.id(), pos_integer()) :: :ok
  def end_timestamp(reporter, track_id, end_timestamp) do
    GenServer.cast(reporter, {:end_timestamp, track_id, end_timestamp})
  end

  @spec rtcp_packet(pid(), Track.id(), SenderReportPacket.t()) :: :ok
  def rtcp_packet(reporter, track_id, rtcp) do
    GenServer.cast(reporter, {:rtcp, track_id, rtcp})
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
    {:ok, %{recording_id: recording_id, tracks: %{}}}
  end

  @impl true
  def handle_cast({:add_track, track, filename}, state) do
    state = put_in(state[:tracks][track.id], {filename, track})
    {:noreply, state}
  end

  @impl true
  def handle_cast({:start_timestamp, track_id, start_timestamp}, state) do
    state =
      update_in(state[:tracks][track_id], fn {filename, track} ->
        track =
          track
          |> Map.put(:start_timestamp, start_timestamp)
          |> Map.put(:end_timestamp, start_timestamp)

        {filename, track}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:end_timestamp, track_id, end_timestamp}, state) do
    state =
      update_in(state[:tracks][track_id], fn {filename, track} ->
        {filename, Map.put(track, :end_timestamp, end_timestamp)}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:rtcp, track_id, rtcp}, state) do
    {filename, track} = get_in(state, [:tracks, track_id])

    track =
      if Map.has_key?(track, :start_timestamp) do
        add_wallclock_start_time(track, rtcp)
      else
        track
      end

    {:noreply, put_in(state, [:tracks, track_id], {filename, track})}
  end

  @impl true
  def handle_call(:get_report, _from, state) do
    tracks_report =
      state.tracks
      |> Map.values()
      |> Enum.reject(fn {_filename, track} ->
        is_nil(track[:start_timestamp]) or is_nil(track[:end_timestamp])
      end)
      |> recalculate_offsets()
      |> Map.new(fn {filename, track} ->
        {filename, Map.take(track, @track_reports_keys)}
      end)

    report = %{recording_id: state.recording_id, tracks: tracks_report}

    {:reply, report, state}
  end

  defp add_wallclock_start_time(track, rtcp) do
    delta_t_ns =
      (rtcp.sender_info.rtp_timestamp - track.start_timestamp) / track.clock_rate * 10 ** 9

    start_timestamp_wallclock = rtcp.sender_info.wallclock_timestamp - delta_t_ns

    Map.put(track, :start_timestamp_wallclock, start_timestamp_wallclock)
  end

  defp recalculate_offsets(tracks) do
    {tracks, _acc} =
      tracks
      |> Enum.sort_by(fn {_filename, track} -> track.offset end)
      |> Enum.map_reduce(nil, fn {filename, track}, acc ->
        cond do
          not Map.has_key?(track, :start_timestamp_wallclock) ->
            {{filename, track}, acc}

          is_nil(acc) ->
            {{filename, track}, track}

          true ->
            offset = acc.offset + track.start_timestamp_wallclock - acc.start_timestamp_wallclock
            {{filename, %{track | offset: offset}}, acc}
        end
      end)

    tracks
  end
end
