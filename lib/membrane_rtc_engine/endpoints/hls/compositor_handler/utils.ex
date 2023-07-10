defmodule Membrane.RTC.Engine.Endpoints.HLS.CompositorHandler.Utils do
  @moduledoc """
  This module calculates `Membrane.VideoCompositor.Transformations` and ` Membrane.VideoCompositor.BaseVideoPlacement` for `Membrane.RTC.Engine.Endpoint.HLS.CompositorHandler`.
  """

  alias Membrane.VideoCompositor.BaseVideoPlacement
  alias Membrane.VideoCompositor.Transformations
  alias Membrane.VideoCompositor.Transformations.{CornersRounding, Cropping}

  @padding 5
  @z_value 0.1
  @corner_radius 20

  @type video_format :: %{width: integer(), height: integer()}

  @spec get_desired_stream_format(pos_integer(), video_format()) :: video_format()
  def get_desired_stream_format(1, output_stream_format) do
    output_stream_format
  end

  def get_desired_stream_format(2, %{width: width, height: height}) do
    %{
      width: round(1 / 2 * width) - @padding * 2,
      height: height - @padding * 2
    }
  end

  def get_desired_stream_format(3, %{width: width, height: height}) do
    %{
      width: round(1 / 2 * width) - @padding * 2,
      height: round(1 / 2 * height) - @padding * 2
    }
  end

  @spec get_placement(
          video_format(),
          pos_integer(),
          non_neg_integer(),
          video_format(),
          video_format()
        ) :: BaseVideoPlacement.t()
  def get_placement(desired_stream_format, 1, 0, input_stream_format, _output_stream_format) do
    %BaseVideoPlacement{
      position: {0, 0},
      size: scale(desired_stream_format, input_stream_format),
      z_value: @z_value
    }
  end

  def get_placement(desired_stream_format, 2, index, input_stream_format, %{width: width}) do
    %BaseVideoPlacement{
      position: {round(1 / 2 * width) * index + @padding, @padding},
      size: scale(desired_stream_format, input_stream_format),
      z_value: @z_value
    }
  end

  def get_placement(desired_stream_format, 3, 2, input_stream_format, %{
        height: height,
        width: width
      }) do
    %BaseVideoPlacement{
      position: {round(1 / 4 * width) + @padding, round(1 / 2 * height) + @padding},
      size: scale(desired_stream_format, input_stream_format),
      z_value: @z_value
    }
  end

  def get_placement(desired_stream_format, 3, index, input_stream_format, %{width: width}) do
    %BaseVideoPlacement{
      position: {round(1 / 2 * width) * index + @padding, @padding},
      size: scale(desired_stream_format, input_stream_format),
      z_value: @z_value
    }
  end

  @spec get_transformations(video_format(), video_format()) :: [Transformations.t()]
  def get_transformations(desired_stream_format, scaled_stream_format) do
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
