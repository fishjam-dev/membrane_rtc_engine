defmodule Membrane.RTC.Engine.Endpoint.HLS.MobileLayoutMaker do
  @moduledoc """
  Module representing function for updating video layout for the HLS stream.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker

  require Membrane.Pad

  alias Membrane.Pad
  alias Membrane.VideoCompositor.RustStructs.BaseVideoPlacement
  alias Membrane.VideoCompositor.VideoTransformations
  alias Membrane.VideoCompositor.VideoTransformations.TextureTransformations.CornersRounding
  alias Membrane.VideoCompositor.VideoTransformations.TextureTransformations.Cropping

  @impl true
  def init(output_caps), do: %{tracks: %{}, output_caps: output_caps}

  @impl true
  def track_added(state, %{metadata: %{"mainPresenter" => true}} = track, caps) do
    new_state = put_in(state, [:tracks, track.id], {track, caps})

    {trans, layout} = get_track_layout(:main, nil, caps, state.output_caps)

    updated_layout = [
      {Pad.ref(:input, track.id), layout}
    ]

    updated_trans = [
      {Pad.ref(:input, track.id), trans}
    ]

    {{updated_layout, updated_trans}, new_state}
  end

  @impl true
  def track_added(state, track, caps) do
    new_state = put_in(state, [:tracks, track.id], {track, caps})
    {update_layout(new_state), new_state}
  end

  @impl true
  def track_removed(state, track) do
    {_, new_state} = pop_in(state, [:tracks, track.id])
    {update_layout(new_state), new_state}
  end

  defp update_layout(%{tracks: tracks, output_caps: output_caps}) do
    tracks
    |> Enum.filter(fn {_id, {track, _caps}} ->
      track.type == :video and not track.metadata["mainPresenter"]
    end)
    |> Enum.with_index()
    |> Enum.flat_map(fn {{_id, {track, caps}}, index} ->
      {track_trans, track_layout} = get_track_layout(:basic, index, caps, output_caps)

      [
        {{Pad.ref(:input, track.id), track_layout}, {Pad.ref(:input, track.id), track_trans}}
      ]
    end)
    |> Enum.unzip()
  end

  defp get_track_layout(:main, _index, caps, output_caps) do
    placement = get_placement(output_caps, caps, {0, 0}, 0.1)
    transformations = get_transformations(output_caps, placement.size, 0)

    {transformations, placement}
  end

  defp get_track_layout(:basic, index, caps, %{width: width, height: height}) do
    output_caps = %{width: round(1 / 2 * width) - 10, height: round(1 / 4 * height) - 10}
    position = {round(index / 2 * width) + 5, height - round(1 / 4 * height) + 5}

    placement = get_placement(output_caps, caps, position, 0.3)
    transformations = get_transformations(output_caps, placement.size, 20)

    {transformations, placement}
  end

  defp video_proportion(%{width: width, height: height}), do: width / height

  defp get_placement(output_caps, caps, position, z_value),
    do: %BaseVideoPlacement{
      position: position,
      size: get_display_size(output_caps, caps),
      z_value: z_value
    }

  defp get_display_size(output_caps, caps) do
    if video_proportion(output_caps) > video_proportion(caps),
      do: {output_caps.width, round(output_caps.width / caps.width * caps.height)},
      else: {round(output_caps.height / caps.height * caps.width), output_caps.height}
  end

  defp get_transformations(output_caps, display_size, radius),
    do: %VideoTransformations{
      texture_transformations: [
        get_cropping(output_caps, display_size),
        get_corners_rounding(radius)
      ]
    }

  defp get_corners_rounding(radius), do: %CornersRounding{border_radius: radius}

  defp get_cropping(output_caps, display_size),
    do: %Cropping{
      crop_top_left_corner: get_cropping_position(output_caps, display_size),
      crop_size: get_cropping_size(output_caps, display_size),
      cropped_video_position: :input_position
    }

  defp get_cropping_position(output_caps, {width, height}) do
    if output_caps.width == width,
      do: {0.0, (height - output_caps.height) / (2 * height)},
      else: {(width - output_caps.width) / (2 * width), 0.0}
  end

  defp get_cropping_size(output_caps, {width, height}) do
    if output_caps.width == width,
      do: {1.0, output_caps.height / height},
      else: {output_caps.width / width, 1.0}
  end
end
