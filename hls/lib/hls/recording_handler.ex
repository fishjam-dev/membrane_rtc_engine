if Code.ensure_loaded?(Membrane.VideoCompositor) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.RecordingHandler do
    @moduledoc """
    Module implementing `Membrane.VideoCompositor.Handler` behaviour.
    Responsible for updating `Membrane.VideoCompositor.Scene` for video compositor.
    This module handles layout for any amount of video tracks.
    """

    @behaviour Membrane.VideoCompositor.Handler

    alias Membrane.VideoCompositor.{BaseVideoPlacement, Scene, VideoConfig}
    alias Membrane.VideoCompositor.Transformations.Cropping

    @padding 5
    @z_value 0.1
    @bar_ratio 5
    @base_position %{width: 0, height: 0}

    @impl true
    def handle_init(ctx) do
      %{output_stream_format: ctx.output_stream_format}
    end

    @impl true
    def handle_info(_msg, _ctx, _state) do
      raise "Not implemented"
    end

    @impl true
    def handle_inputs_change(
          inputs,
          _ctx,
          %{output_stream_format: output_stream_format} = state
        ) do
      split_inputs =
        Enum.split_with(inputs, fn {_ref, %{metadata: metadata}} ->
          Map.get(metadata, "type") == "screenshare"
        end)

      # WINDOW
      # A key component that serves as the canvas for positioning and calculating layout elements
      # such as grid, bar, or screen share
      # Changing the window's position or resolution can modify the bar's position or size.

      videos_config =
        case split_inputs do
          {[], []} ->
            []

          {[], videos} ->
            grid = videos |> length() |> calculate_grid()
            videos_config_grid(videos, output_stream_format, @base_position, grid)

          {[screenshare], []} ->
            screenshare_config(screenshare, output_stream_format, @base_position)

          {[screenshare], videos} ->
            videos_window_resolution = %{
              width: output_stream_format.width,
              height: div(output_stream_format.height, @bar_ratio)
            }

            sh_window_resolution = %{
              width: output_stream_format.width,
              height: output_stream_format.height - videos_window_resolution.height
            }

            sh_window_position = %{width: 0, height: videos_window_resolution.height}

            videos_config_bar(videos, videos_window_resolution, @base_position) ++
              screenshare_config(screenshare, sh_window_resolution, sh_window_position)

          {_screenshares, _videos} ->
            raise "The recording handler accepts only a single screen share"
        end

      {to_scene(videos_config), state}
    end

    # VIDEO BAR
    # Activated when a screen share is occurring.
    # Accommodates all non-screen share videos in a top bar layout.
    #
    # Example layouts:
    #
    # 1) One presenter
    #      _ _ _ _ _ _ _ _ _
    #     |______|_1_|______|
    #     |                 |
    #     |  screen share   |
    #     |                 |
    #     |                 |
    #      - - - - - - - - -
    # 2) Two presenters
    #      _ _ _ _ _ _ _ _ _
    #     |___|_1_|_|_2_|___|
    #     |                 |
    #     |  screen share   |
    #     |                 |
    #     |                 |
    #      - - - - - - - - -

    defp videos_config_bar(videos, window_resolution, window_position) do
      column_width = div(window_resolution.width, length(videos))

      desired_resolution = %{
          width: column_width - @padding * 2,
          height: window_resolution.height - @padding * 2
        }

      scaled_resolutions =
        Enum.map(videos, fn {_ref, video} -> scale(desired_resolution, video.stream_format) end)

      positions = centralized_positions(scaled_resolutions, window_resolution, window_position)

      video_configs = to_video_configs(scaled_resolutions, positions)

      videos
      |> Enum.map(fn {ref, _video} -> ref end)
      |> Enum.zip(video_configs)
    end

    # SCREEN SHARE
    # Engaged during a screen share session.
    # If the screen share's resolution is smaller than the window's, it will not be scaled.
    # Instead, it will be centralized and cropped, if necessary.
    # Note: Only one screen share can be active at a time.
    #
    # Example layouts (screen share is narrower than window):
    #
    # 1) No presenter
    #      _ _ _ _ _ _ _ _ _
    #     | |             | |
    #     | |             | |
    #     | |screen share | |
    #     | |             | |
    #     | |             | |
    #      - - - - - - - - -
    # 2) Two presenters
    #      _ _ _ _ _ _ _ _ _
    #     |___|_1_|_|_2_|___|
    #     | |             | |
    #     | |screen share | |
    #     | |             | |
    #     | |             | |
    #      - - - - - - - - -

    defp screenshare_config({ref, screenshare}, window_resolution, window_position) do
      desired_resolution = %{
        width: window_resolution.width - @padding * 2,
        height: window_resolution.height - @padding * 2
      }

      scaled_resolution = screenshare_resolution(desired_resolution, screenshare)

      placement =
        screenshare_placement(
          screenshare,
          scaled_resolution,
          desired_resolution,
          window_position
        )

      transformation = get_transformations(scaled_resolution, screenshare.stream_format)

      [{ref, %VideoConfig{placement: placement, transformations: transformation}}]
    end

    # VIDEO GRID
    # Activated when a screen share is not in progress.
    # Organizes all videos into a grid layout.
    #
    # Example layouts:
    #
    # 1) One presenter
    #      _ _ _ _ _ _ _ _ _
    #     |                 |
    #     |                 |
    #     |        1        |
    #     |                 |
    #     |                 |
    #      - - - - - - - - -
    # 2) Two presenters
    #      _ _ _ _ _ _ _ _ _
    #     |        |        |
    #     |        |        |
    #     |   1    |    2   |
    #     |        |        |
    #     |        |        |
    #      - - - - - - - - -
    # 3) Three presenters
    #      _ _ _ _ _ _ _ _ _
    #     |        |        |
    #     |   1    |   2    |
    #     | _ _ _ _|_ _ _ _ |
    #     |    |       |    |
    #     |    |   3   |    |
    #      - - - - - - - - -

    defp videos_config_grid(
           videos,
           window_resolution,
           window_position,
           {max_videos_in_row, row_amount}
         ) do
      output_col_width = div(window_resolution.width, max_videos_in_row)
      output_row_height = div(window_resolution.height, row_amount)

      desired_resolution = %{
        width: output_col_width - @padding * 2,
        height: output_row_height - @padding * 2
      }

      videos
      |> Enum.chunk_every(max_videos_in_row)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row_videos, row_index} ->
        videos_in_row = length(row_videos)

        row_videos
        |> Enum.with_index()
        |> Enum.map(fn {{ref, %{stream_format: video_format}}, col_index} ->
          scaled_resolution = scale(desired_resolution, video_format)

          position =
            calculate_position(
              {row_index, col_index},
              {output_col_width, output_row_height},
              max_videos_in_row - videos_in_row,
              desired_resolution,
              scaled_resolution,
              window_position
            )

          placement = %BaseVideoPlacement{
            position: position,
            size: {scaled_resolution.width, scaled_resolution.height},
            z_value: @z_value
          }

          # Compositor works in two steps.
          # First it scales and places video creating output in `scaled_video_format`.
          # Then it does tranformations like cropping or cutting video.
          # In transformation step compositor works on `scaled_video_format` from previous step.
          transformations = get_transformations(desired_resolution, scaled_resolution)

          {ref,
           %VideoConfig{
             placement: placement,
             transformations: transformations
           }}
        end)
      end)
    end

    ###
    ### VIDEO BAR FUNCTIONS
    ###

    defp to_video_configs(scaled_resolutions, positions) do
      scaled_resolutions
      |> Enum.zip(positions)
      |> Enum.map(fn {%{width: width, height: height}, position} ->
        placement = %BaseVideoPlacement{
          position: position,
          size: {width, height},
          z_value: @z_value
        }

        %VideoConfig{
          placement: placement,
          transformations: []
        }
      end)
    end

    defp centralized_positions(scaled_resolutions, window_resolution, window_position) do
      # In this step, videos are sequenced consecutively, beginning from the window_position.
      # The total width embodies the combined width of all sequentially placed videos.
      {videos_position, summed_width} =
        Enum.map_reduce(scaled_resolutions, window_position.width, fn %{width: width},
                                                                      summed_width ->
          video_position = {
            # width
            summed_width + @padding,
            # height
            window_position.height + @padding
          }

          {video_position, summed_width + width + @padding}
        end)

      # Focusing on centering the videos on the bar.
      # Calculation computes the displacement required to shift the videos towards the center.
      center_width = div(window_resolution.width - summed_width, 2)

      centralized_positions =
        Enum.map(videos_position, fn {width, height} -> {width + center_width, height} end)

      centralized_positions
    end

    ###
    ### SCREENSHARE FUNCTIONS
    ###

    defp screenshare_resolution(desired_format, screenshare) do
      %{
        width: min(desired_format.width, screenshare.stream_format.width),
        height: min(desired_format.height, screenshare.stream_format.height)
      }
    end

    defp screenshare_placement(
           %{stream_format: stream_format} = _screenshare,
           %{width: scaled_width, height: scaled_height},
           %{width: desired_width, height: desired_height},
           window_position
         ) do
      width = div(desired_width - scaled_width, 2) + window_position.width + @padding
      height = div(desired_height - scaled_height, 2) + window_position.height + @padding

      %BaseVideoPlacement{
        position: {width, height},
        # Aim to maintain screen share quality by avoiding scaling.
        # Extra parts extending beyond the 'window' will be cropped instead.
        size: {stream_format.width, stream_format.height},
        z_value: @z_value
      }
    end

    ###
    ### VIDEO GRID FUNCTIONS
    ###

    defp calculate_grid(inputs_amount) do
      max_inputs_in_row = inputs_amount |> :math.sqrt() |> ceil()
      row_amount = ceil(inputs_amount / max_inputs_in_row)

      {max_inputs_in_row, row_amount}
    end

    defp calculate_position(
           {row_index, col_index},
           {output_col_width, output_row_height},
           # difference between number of columns in row and videos in that row
           inputs_difference,
           desired_resolution,
           scaled_resolution,
           window_position
         ) do
      # Calculates the difference between the desired resolution and the scaled resolution.
      # This difference is used to center each video within its corresponding grid cell.
      width_offset = max(div(desired_resolution.width - scaled_resolution.width, 2), 0)
      height_offset = max(div(desired_resolution.height - scaled_resolution.height, 2), 0)

      width =
        calculate_placement_width(
          col_index,
          output_col_width,
          inputs_difference,
          width_offset,
          window_position
        )

      height =
        calculate_placement_height(
          row_index,
          output_row_height,
          height_offset,
          window_position
        )

      {width, height}
    end

    defp calculate_placement_height(row_index, output_row_height, height_offset, window_position) do
      window_position.height + row_index * output_row_height + height_offset + @padding
    end

    defp calculate_placement_width(
           col_index,
           output_col_width,
           inputs_difference,
           width_offset,
           window_position
         ) do
      # Center the tiles if we have less of them than max_inputs_in_row
      center_offset = div(output_col_width * inputs_difference, 2)

      window_position.width + col_index * output_col_width + width_offset + center_offset +
        window_position.width
    end

    ###
    ### SHARED FUNCTIONS
    ###

    defp scale(desired_resolution, input_resolution) do
      if video_proportion(desired_resolution) < video_proportion(input_resolution) do
        width_proportion = desired_resolution.width / input_resolution.width
        height = width_proportion * input_resolution.height

        %{width: desired_resolution.width, height: round(height)}
      else
        height_proportion = desired_resolution.height / input_resolution.height
        width = height_proportion * input_resolution.width

        %{width: round(width), height: desired_resolution.height}
      end
    end

    defp video_proportion(%{width: width, height: height}), do: width / height

    defp get_transformations(desired_resolution, scaled_resolution) do
      [get_cropping(desired_resolution, scaled_resolution)]
    end

    defp get_cropping(desired_resolution, scaled_resolution),
      do: %Cropping{
        crop_top_left_corner: get_cropping_position(desired_resolution, scaled_resolution),
        crop_size: get_cropping_size(desired_resolution, scaled_resolution),
        cropped_video_position: :input_position
      }

    defp get_cropping_position(
           desired_resolution,
           %{width: width, height: height} = _scaled_resolution
         ) do
      {max(0.0, width - desired_resolution.width) / (2 * width),
       max(0.0, height - desired_resolution.height) / (2 * height)}
    end

    defp get_cropping_size(
           desired_resolution,
           %{width: width, height: height} = _scaled_resolution
         ) do
      {min(1.0, desired_resolution.width / width), min(1.0, desired_resolution.height / height)}
    end

    defp to_scene(videos_config) do
      videos_config
      |> Enum.into(%{})
      |> then(fn video_configs -> %Scene{video_configs: video_configs} end)
    end
  end
end
