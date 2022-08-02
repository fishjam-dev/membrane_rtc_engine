defmodule Membrane.RTC.Engine.BandwidthManager do
  @moduledoc false

  use Bunch.Access

  alias Membrane.RTC.Engine.Track

  defstruct track_bandwidth: %{}, endpoint_bandwidth: %{}

  @type endpoint_id() :: any()
  @type layer() :: :l | :m | :h

  @type track_estimation() :: %{
          layer() => number()
        }

  @opaque t() :: %__MODULE__{
            track_bandwidth: %{Track.id() => number()},
            endpoint_bandwidth: %{endpoint_id() => number()}
          }

  @spec new() :: t()
  def new(), do: %__MODULE__{}

  @spec add_endpoint_estimation(t(), any(), number()) :: t()
  def add_endpoint_estimation(%__MODULE__{} = state, endpoint_id, estimation) do
    put_in(state, [:endpoint_bandwidth, endpoint_id], estimation)
  end

  @spec add_track_estimation(t(), Track.id(), track_estimation()) :: t()
  def add_track_estimation(%__MODULE__{} = state, track_id, estimation) do
    put_in(state, [:track_bandwidth, track_id], estimation)
  end

  @spec generate_allowed_layers(t(), any()) :: %{
          {endpoint_id(), Track.id()} => list(:h | :m | :l)
        }
  def generate_allowed_layers(%__MODULE__{} = state, subscriptions) do
    subscriptions
    |> Enum.flat_map(fn {endpoint_id, meta} ->
      tracks =
        meta
        |> Map.keys()
        |> Enum.map(fn track ->
          {track, Map.get(state.track_bandwidth, track, %{"l" => 0, "m" => 0, "h" => 0})}
        end)

      endpoint_estimation = Map.get(state.endpoint_bandwidth, endpoint_id, :infinity)

      endpoint_estimation
      |> divide_bandwidth(tracks)
      |> Enum.map(fn {track_id, layers} -> {{endpoint_id, track_id}, Map.keys(layers)} end)
    end)
    |> Map.new()
  end

  defp divide_bandwidth(bandwidth, tracks) do
    get_bandwidth = fn
      %{"h" => h} -> h
      %{"m" => m} -> m
      %{"l" => l} -> l
      _otherwise -> 0
    end

    drop_highest_layer = fn values ->
      Enum.reduce_while(
        ["h", "m", "l"],
        values,
        &if(Map.has_key?(&2, &1), do: {:halt, Map.delete(&2, &1)}, else: {:cont, &2})
      )
    end

    total_tracks_bandwidth =
      tracks
      |> Keyword.values()
      |> Enum.map(get_bandwidth)
      |> Enum.sum()

    if total_tracks_bandwidth <= bandwidth do
      tracks
    else
      tracks =
        tracks
        |> Enum.sort_by(
          fn {_key, values} ->
            a = get_bandwidth.(values)
            b = values |> then(drop_highest_layer) |> then(get_bandwidth)

            (a - b) / map_size(values)
          end,
          :asc
        )
        |> Enum.map(fn {key, values} -> {key, drop_highest_layer.(values)} end)

      divide_bandwidth(bandwidth, tracks)
    end
  end
end
