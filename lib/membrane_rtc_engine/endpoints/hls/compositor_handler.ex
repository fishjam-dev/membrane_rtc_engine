if Code.ensure_loaded?(Membrane.VideoCompositor) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.CompositorHandler do
    @moduledoc false

    @behaviour Membrane.VideoCompositor.Handler

    require Membrane.Pad

    alias Membrane.Pad
    alias Membrane.VideoCompositor.{BaseVideoPlacement, Scene, VideoConfig}
    alias Membrane.VideoCompositor.Transformations.{CornersRounding, Cropping}

    @padding 5
    @z_value 0.1
    @corner_radius 20

    @impl true
    def handle_init(ctx) do
      %{output_stream_format: ctx.output_stream_format, screenShare?: false}
    end

    @impl true
    def handle_info(_msg, _ctx, _state) do
      raise "Not implemented"
    end

    @impl true
    def handle_inputs_change(
          inputs,
          _ctx,
          %{screenShare?: false} = state
        ) do
      blank? = Map.has_key?(inputs, Pad.ref(:input, :blank))

      inputs = Enum.filter(inputs, fn {pad_ref, _} -> pad_ref != Pad.ref(:input, :blank) end)
      inputs_number = Enum.count(inputs)

      scene =
        inputs
        |> Enum.with_index()
        |> Enum.map(fn {{ref, %{stream_format: stream_format}}, index} ->
          video_config = get_video_config(stream_format, inputs_number, index, state)

          {ref, video_config}
        end)
        |> Enum.into(%{})
        |> maybe_add_blank_config(blank?)
        |> then(fn video_configs -> %Scene{video_configs: video_configs} end)

      {scene, state}
    end

    @impl true
    def handle_inputs_change(
          inputs,
          _ctx,
          %{screenShare?: true} = state
        ) do
      {inputs, state}
    end

    defp get_video_config(input_stream_format, inputs_number, index, %{
           output_stream_format: output_stream_format
         }) do
      desired_stream_format = get_desired_stream_format(inputs_number, output_stream_format)

      placement =
        get_placement(
          desired_stream_format,
          inputs_number,
          index,
          input_stream_format,
          output_stream_format
        )

      scaled_stream_format = placement.size

      transformations =
        get_transformations(desired_stream_format, scaled_stream_format, @corner_radius)

      %VideoConfig{
        placement: placement,
        transformations: transformations
      }
    end

    defp get_desired_stream_format(1, output_stream_format) do
      output_stream_format
    end

    defp get_desired_stream_format(2, %{width: width, height: height}) do
      %{
        width: round(1 / 2 * width) - @padding * 2,
        height: height - @padding * 2
      }
    end

    defp get_desired_stream_format(3, %{width: width, height: height}) do
      %{
        width: round(1 / 2 * width) - @padding * 2,
        height: round(1 / 2 * height) - @padding * 2
      }
    end

    defp get_placement(desired_stream_format, 1, 0, input_stream_format, _output_stream_format) do
      %BaseVideoPlacement{
        position: {0, 0},
        size: scale(desired_stream_format, input_stream_format),
        z_value: @z_value
      }
    end

    defp get_placement(desired_stream_format, 2, index, input_stream_format, %{width: width}) do
      %BaseVideoPlacement{
        position: {round(1 / 2 * width) * index + @padding, @padding},
        size: scale(desired_stream_format, input_stream_format),
        z_value: @z_value
      }
    end

    defp get_placement(desired_stream_format, 3, 2, input_stream_format, %{
           height: height,
           width: width
         }) do
      %BaseVideoPlacement{
        position: {round(1 / 4 * width) + @padding, round(1 / 2 * height) + @padding},
        size: scale(desired_stream_format, input_stream_format),
        z_value: @z_value
      }
    end

    defp get_placement(desired_stream_format, 3, index, input_stream_format, %{width: width}) do
      %BaseVideoPlacement{
        position: {round(1 / 2 * width) * index + @padding, @padding},
        size: scale(desired_stream_format, input_stream_format),
        z_value: @z_value
      }
    end

    defp scale(desired_stream_format, input_stream_format) do
      if video_proportion(desired_stream_format) > video_proportion(input_stream_format),
        do:
          {desired_stream_format.width,
           round(
             desired_stream_format.width / input_stream_format.width * input_stream_format.height
           )},
        else:
          {round(
             desired_stream_format.height / input_stream_format.height * input_stream_format.width
           ), desired_stream_format.height}
    end

    defp get_transformations(desired_stream_format, scaled_stream_format, radius) do
      [
        get_cropping(desired_stream_format, scaled_stream_format),
        get_corners_rounding(radius)
      ]
    end

    defp video_proportion(%{width: width, height: height}), do: width / height

    defp get_corners_rounding(radius), do: %CornersRounding{border_radius: radius}

    defp get_cropping(desired_stream_format, scaled_stream_format),
      do: %Cropping{
        crop_top_left_corner: get_cropping_position(desired_stream_format, scaled_stream_format),
        crop_size: get_cropping_size(desired_stream_format, scaled_stream_format),
        cropped_video_position: :input_position
      }

    defp get_cropping_position(desired_stream_format, {width, height}) do
      if desired_stream_format.width == width,
        do: {0.0, (height - desired_stream_format.height) / (2 * height)},
        else: {(width - desired_stream_format.width) / (2 * width), 0.0}
    end

    defp get_cropping_size(desired_stream_format, {width, height}) do
      if desired_stream_format.width == width,
        do: {1.0, desired_stream_format.height / height},
        else: {desired_stream_format.width / width, 1.0}
    end

    defp maybe_add_blank_config(video_configs, true) do
      Map.merge(video_configs, blank_config())
    end

    defp maybe_add_blank_config(video_configs, false) do
      video_configs
    end

    defp blank_config(),
      do: %{
        Pad.ref(:input, :blank) => %VideoConfig{
          placement: %BaseVideoPlacement{
            position: {0, 0},
            size: {100, 100},
            z_value: 0.0
          }
        }
      }
  end
end
