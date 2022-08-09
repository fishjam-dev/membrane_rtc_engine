defmodule Membrane.RTC.Engine.Support.DumpEndpoint do
  @moduledoc false

  # Endpoint that subscribes on any track and dumps it to a file.

  use Membrane.Bin

  alias Membrane.RTC.Engine
  alias Membrane.Stream.Serializer
  alias Membrane.File.Sink

  require Membrane.Logger

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              directory_path: [
                spec: Path.t(),
                description: """
                Path to directory, where buffers from tracks will be stored.
                Each track will be stored in seperate file.
                """
              ],
              format: [
                spec: atom(),
                default: nil,
                description: """
                Track format this endpoint will subscribe for. Pass `nil` to subscribe for each track.
                """
              ],
              owner: [
                spec: pid(),
                description: "Pid of parent all notifications will be send to."
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  @impl true
  def handle_init(opts) do
    state = %{
      directory_path: opts.directory_path,
      format: opts.format,
      rtc_engine: opts.rtc_engine,
      owner: opts.owner
    }

    {:ok, state}
  end

  @impl true
  def handle_element_end_of_stream({{:sink, _track_id}, :input}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name
    send(state.owner, {:end_of_stream, endpoint_id})
    {:ok, state}
  end

  @impl true
  def handle_element_end_of_stream(_element, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:input, track_id) = pad, _ctx, state) do
    children = %{
      {:serializer, track_id} => Serializer,
      {:sink, track_id} => %Sink{location: Path.join(state.directory_path, track_id)}
    }

    links = [
      link_bin_input(pad)
      |> to({:serializer, track_id})
      |> to({:sink, track_id})
    ]

    {{:ok, spec: %ParentSpec{children: children, links: links}}, state}
  end

  @impl true
  def handle_other({:new_tracks, tracks}, ctx, state) do
    {:endpoint, endpoint_id} = ctx.name

    tracks =
      if state.format != nil,
        do: Enum.filter(tracks, fn track -> state.format in track.format end),
        else: tracks

    Enum.reduce_while(tracks, {:ok, state}, fn track, {:ok, state} ->
      format = state.format || List.first(track.format)

      case Engine.subscribe(state.rtc_engine, endpoint_id, track.id, format) do
        :ok ->
          {:cont, {:ok, state}}

        {:error, :invalid_track_id} ->
          Membrane.Logger.warn("""
          Couldn't subscribe to track: #{inspect(track.id)}. No such track.
          It had to be removed just after publishing it. Ignoring.
          """)

          {:cont, {:ok, state}}

        {:error, reason} ->
          raise "Couldn't subscribe for track: #{inspect(track.id)}. Reason: #{inspect(reason)}"
      end
    end)
  end

  @impl true
  def handle_other({:remove_tracks, []}, _ctx, state) do
    {:ok, state}
  end
end
