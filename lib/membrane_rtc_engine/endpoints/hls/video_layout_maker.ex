defmodule Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker do
  @moduledoc """
  Module representing function for updating video layout for the HLS stream.
  """
  require Membrane.Pad

  alias Membrane.VideoCompositor.RustStructs.VideoPlacement
  alias Membrane.Pad

  def update_layout(state, curr_track) do
    placements =
      state.tracks
      |> Enum.filter(fn {id, track} ->
        track.type == :video and id != curr_track.id and not track.metadata["mainPresenter"]
      end)
      |> Enum.with_index()
      |> Enum.flat_map(fn {{_id, track}, index} ->
        track_layout = get_track_layout(track, index, state.mixer_config.video.caps)
        blank_layout = get_track_layout(:blank, index, state.mixer_config.video.caps)

        [
          {Pad.ref(:input, track.id), track_layout},
          {Pad.ref(:input, {:blank, track.id}), blank_layout}
        ]
      end)

    if placements == [],
      do: [],
      else: [forward: {:compositor, {:update_placement, placements}}]
  end

  def get_track_layout(:blank, index, %{width: width, height: height}) do
    position = {round(index / 2 * width), height - round(1 / 4 * height)}
    display_size = {round(1 / 2 * width), round(1 / 4 * height)}

    %VideoPlacement{
      position: position,
      display_size: display_size,
      z_value: 0.2
    }
  end

  def get_track_layout(%{"mainPresenter" => true}, _index, %{width: width, height: height}),
    do: %VideoPlacement{
      position: {0, 0},
      display_size: {width, height},
      z_value: 0.1
    }

  def get_track_layout(_track, index, %{width: width, height: height}) do
    position = {round(index / 2 * width) + 5, height - round(1 / 4 * height) + 5}
    display_size = {round(1 / 2 * width) - 10, round(1 / 4 * height) - 10}

    %VideoPlacement{
      position: position,
      display_size: display_size,
      z_value: 0.3
    }
  end
end
