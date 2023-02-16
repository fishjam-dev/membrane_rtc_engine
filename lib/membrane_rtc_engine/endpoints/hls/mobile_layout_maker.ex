if Code.ensure_loaded?(Membrane.VideoCompositor) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.MobileLayoutMaker do
    @moduledoc """
    Module representing function for updating video layout for the HLS stream.

    1) Only main presenter
         _ _ _ _
        |       |
        |       |
        |       |
        |       |
        |       |
         - - - -
    2) Main presenter and one side presenter
         _ _ _ _
        |       |
        |       |
        |       |
         - -    |
        |   |   |
         - - - -

    3) Main presenter and two side presenters
         _ _ _ _
        |       |
        |       |
        |       |
         - - - -
        |   |   |
         - - - -
    """

    @behaviour Membrane.RTC.Engine.Endpoint.HLS.VideoLayoutMaker

    alias Membrane.VideoCompositor.RustStructs.BaseVideoPlacement
    alias Membrane.VideoCompositor.VideoTransformations
    alias Membrane.VideoCompositor.VideoTransformations.TextureTransformations.CornersRounding
    alias Membrane.VideoCompositor.VideoTransformations.TextureTransformations.Cropping

    @main_stream %{position: {0, 0}, z_value: 0.1, corner_radius: 0}
    @side_stream %{z_value: 0.3, corner_radius: 20, padding: 5}

    @impl true
    def init(output_stream_format), do: %{tracks: %{}, output_stream_format: output_stream_format}

    @impl true
    def track_added(%{metadata: %{"mainPresenter" => true}} = track, stream_format, state) do
      new_state = put_in(state, [:tracks, track.id], {track, stream_format})

      {layout, transformations} =
        get_track_layout(:main, nil, stream_format, state.output_stream_format)

      updated_layout = [
        {track.id, layout, transformations}
      ]

      {updated_layout, new_state}
    end

    @impl true
    def track_added(track, stream_format, state) do
      new_state = put_in(state, [:tracks, track.id], {track, stream_format})
      {update_layout(new_state), new_state}
    end

    @impl true
    def track_updated(track, stream_format, state), do: track_added(track, stream_format, state)

    @impl true
    def track_removed(track, state) do
      {_, new_state} = pop_in(state, [:tracks, track.id])
      {update_layout(new_state), new_state}
    end

    defp update_layout(%{tracks: tracks, output_stream_format: output_stream_format}) do
      tracks
      |> Enum.filter(fn {_id, {track, _stream_format}} ->
        track.type == :video and not track.metadata["mainPresenter"]
      end)
      |> Enum.with_index()
      |> Enum.flat_map(fn {{_id, {track, stream_format}}, index} ->
        {layout, transcoding} =
          get_track_layout(:side, index, stream_format, output_stream_format)

        [
          {track.id, layout, transcoding}
        ]
      end)
    end

    defp get_track_layout(:main, _index, stream_format, output_stream_format) do
      placement =
        get_placement(
          output_stream_format,
          stream_format,
          @main_stream.position,
          @main_stream.z_value
        )

      transformations =
        get_transformations(output_stream_format, placement.size, @main_stream.corner_radius)

      {placement, transformations}
    end

    defp get_track_layout(:side, index, stream_format, %{width: width, height: height}) do
      # side track layout is a thumbnail that is placed on the bottom of the main stream.
      output_stream_format = %{
        width: round(1 / 2 * width) - @side_stream.padding * 2,
        height: round(1 / 4 * height) - @side_stream.padding * 2
      }

      # x coordinate of a thumbnail can differ depends on which index they have, y coordinate is always the same.
      position_x = round(index / 2 * width) + @side_stream.padding
      position_y = height - round(1 / 4 * height) + @side_stream.padding

      position = {position_x, position_y}

      placement =
        get_placement(output_stream_format, stream_format, position, @side_stream.z_value)

      transformations =
        get_transformations(output_stream_format, placement.size, @side_stream.corner_radius)

      {placement, transformations}
    end

    defp video_proportion(%{width: width, height: height}), do: width / height

    defp get_placement(output_stream_format, stream_format, position, z_value),
      do: %BaseVideoPlacement{
        position: position,
        size: get_display_size(output_stream_format, stream_format),
        z_value: z_value
      }

    defp get_display_size(output_stream_format, stream_format) do
      if video_proportion(output_stream_format) > video_proportion(stream_format),
        do:
          {output_stream_format.width,
           round(output_stream_format.width / stream_format.width * stream_format.height)},
        else:
          {round(output_stream_format.height / stream_format.height * stream_format.width),
           output_stream_format.height}
    end

    defp get_transformations(output_stream_format, display_size, radius),
      do: %VideoTransformations{
        texture_transformations: [
          get_cropping(output_stream_format, display_size),
          get_corners_rounding(radius)
        ]
      }

    defp get_corners_rounding(radius), do: %CornersRounding{border_radius: radius}

    defp get_cropping(output_stream_format, display_size),
      do: %Cropping{
        crop_top_left_corner: get_cropping_position(output_stream_format, display_size),
        crop_size: get_cropping_size(output_stream_format, display_size),
        cropped_video_position: :input_position
      }

    defp get_cropping_position(output_stream_format, {width, height}) do
      if output_stream_format.width == width,
        do: {0.0, (height - output_stream_format.height) / (2 * height)},
        else: {(width - output_stream_format.width) / (2 * width), 0.0}
    end

    defp get_cropping_size(output_stream_format, {width, height}) do
      if output_stream_format.width == width,
        do: {1.0, output_stream_format.height / height},
        else: {output_stream_format.width / width, 1.0}
    end
  end
end
