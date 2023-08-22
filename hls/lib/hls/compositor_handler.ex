if Code.ensure_loaded?(Membrane.VideoCompositor) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorHandler do
    @moduledoc """
    Module implementing `Membrane.VideoCompositor.Handler` behaviour.
    Responsible for updating `Membrane.VideoCompositor.Scene` for video compositor.
    This module handles layout for any amount of video tracks.

    Example layouts:
    ```ascii
    1) One presenter
         _ _ _ _ _ _ _ _ _
        |                 |
        |                 |
        |        1        |
        |                 |
        |                 |
         - - - - - - - - -
    2) Two presenters
         _ _ _ _ _ _ _ _ _
        |        |        |
        |        |        |
        |   1    |    2   |
        |        |        |
        |        |        |
         - - - - - - - - -
    3) Three presenters
         _ _ _ _ _ _ _ _ _
        |        |        |
        |   1    |   2    |
        | _ _ _ _|_ _ _ _ |
        |    |       |    |
        |    |   3   |    |
         - - - - - - - - -
    ```
    """

    @behaviour Membrane.VideoCompositor.Handler

    alias Membrane.VideoCompositor.{BaseVideoPlacement, Scene, VideoConfig}
    alias Membrane.VideoCompositor.Transformations.{CornersRounding, Cropping}

    @padding 5
    @z_value 0.1
    @corner_radius 20

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
        )
        when map_size(inputs) > 0 do
      inputs_amount = Enum.count(inputs)

      max_inputs_in_row = :math.sqrt(inputs_amount) |> ceil()
      row_amount = ceil(inputs_amount / max_inputs_in_row)

      output_row_height = div(output_stream_format.height, row_amount)
      output_col_width = div(output_stream_format.width, max_inputs_in_row)

      desired_stream_format = %{
        width: output_col_width - @padding * 2,
        height: output_row_height - @padding * 2
      }

      scene =
        Enum.chunk_every(inputs, max_inputs_in_row)
        |> Enum.with_index()
        |> Enum.flat_map(fn {row_inputs, row_index} ->
          placement_height = row_index * output_row_height + @padding
          inputs_in_row = Enum.count(row_inputs)

          Enum.with_index(row_inputs)
          |> Enum.map(fn {{ref, %{stream_format: stream_format}}, col_index} ->
            # Center the tiles if we have less of them than max_inputs_in_row
            width_offset = div(output_col_width * (max_inputs_in_row - inputs_in_row), 2)
            placement_width = col_index * output_col_width + @padding + width_offset

            placement = %BaseVideoPlacement{
              position: {placement_width, placement_height},
              size: scale(desired_stream_format, stream_format),
              z_value: @z_value
            }

            {scaled_width, scaled_height} = placement.size
            scaled_stream_format = %{width: scaled_width, height: scaled_height}

            # Compositor works in two steps.
            # First it scales and places video creating output in `scaled_stream_format`.
            # Then it does tranformations like cropping or cutting video.
            # In transformation step compositor works on `scaled_stream_format` from previous step.
            transformations = get_transformations(desired_stream_format, scaled_stream_format)

            {ref,
             %VideoConfig{
               placement: placement,
               transformations: transformations
             }}
          end)
        end)
        |> Enum.into(%{})
        |> then(fn video_configs -> %Scene{video_configs: video_configs} end)

      {scene, state}
    end

    @impl true
    def handle_inputs_change(_inputs, _ctx, state) do
      {%Scene{video_configs: %{}}, state}
    end

    defp get_transformations(desired_stream_format, scaled_stream_format) do
      [
        get_cropping(desired_stream_format, scaled_stream_format),
        get_corners_rounding(@corner_radius)
      ]
    end

    defp scale(desired_stream_format, input_stream_format) do
      if video_proportion(desired_stream_format) > video_proportion(input_stream_format) do
        width_proportion = desired_stream_format.width / input_stream_format.width
        height = width_proportion * input_stream_format.height

        {desired_stream_format.width, round(height)}
      else
        height_proportion = desired_stream_format.height / input_stream_format.height
        width = height_proportion * input_stream_format.width

        {round(width), desired_stream_format.height}
      end
    end

    defp video_proportion(%{width: width, height: height}), do: width / height

    defp get_corners_rounding(radius), do: %CornersRounding{border_radius: radius}

    defp get_cropping(desired_stream_format, scaled_stream_format),
      do: %Cropping{
        crop_top_left_corner: get_cropping_position(desired_stream_format, scaled_stream_format),
        crop_size: get_cropping_size(desired_stream_format, scaled_stream_format),
        cropped_video_position: :input_position
      }

    defp get_cropping_position(desired_stream_format, %{width: width, height: height}) do
      if desired_stream_format.width == width,
        do: {0.0, (height - desired_stream_format.height) / (2 * height)},
        else: {(width - desired_stream_format.width) / (2 * width), 0.0}
    end

    defp get_cropping_size(desired_stream_format, %{width: width, height: height}) do
      if desired_stream_format.width == width,
        do: {1.0, desired_stream_format.height / height},
        else: {desired_stream_format.width / width, 1.0}
    end
  end
end
