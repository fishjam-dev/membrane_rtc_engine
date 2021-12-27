defmodule Membrane.RTC.Engine.Endpoint.SpeakersDetector do
  @moduledoc false

  use Membrane.Bin
  require Membrane.Logger

  def_options ets_name: [
                spec: String.t(),
                description: "Name of ets table",
                default: "room"
              ]

  @interval 5_000

  @impl true
  def handle_init(opts) do
    ets_name = :"#{opts.ets_name}"
    :ets.new(ets_name, [:set, :public, :named_table])
    Process.send_after(self(), :check_vad, @interval)
    {:ok, %{tracks: %{}, ets_name: ets_name}}
  end

  @impl true
  def handle_other({:new_tracks, tracks}, _ctx, state) do
    new_tracks =
      tracks
      |> Enum.filter(&(&1.type == :audio))
      |> Map.new(&{&1.id, &1})

    {:ok, Map.update!(state, :tracks, &Map.merge(&1, new_tracks))}
  end

  @impl true
  def handle_other(:check_vad, _ctx, state) do
    prioritized_tracks =
      state.tracks
      |> Enum.map(fn {track_id, track} ->
        value =
          case :ets.lookup(state.ets_name, track_id) do
            [{_track_id, value} | _] -> value
            [] -> 0
          end

        new_value =
          if value == 0, do: 0, else: :ets.update_counter(state.ets_name, track_id, {2, 1})

        {track, new_value}
      end)
      |> Enum.sort_by(fn {_track, value} -> value end, &>=/2)
      |> Enum.map(fn {track, _value} -> track end)

    Process.send_after(self(), :check_vad, @interval)
    {{:ok, notify: {:publish, {:prioritized_tracks, prioritized_tracks}}}, state}
  end

  @impl true
  def handle_other(msg, _ctx, state) do
    Membrane.Logger.warn("Unexpected message: #{inspect(msg)}. Ignoring.")
    {:ok, state}
  end
end
