defmodule Membrane.H264.FFmpeg.Common do
  @moduledoc false
  use Ratio
  @h264_time_base 90_000
  @no_pts -9_223_372_036_854_775_808

  @doc """
  Converts time in membrane time base (1 [ns]) to h264 time base (1/90_000 [s])
  """
  @spec to_h264_time_base_truncated(number | Ratio.t() | nil) :: integer
  def to_h264_time_base_truncated(nil), do: @no_pts

  def to_h264_time_base_truncated(timestamp) do
    (timestamp * @h264_time_base / Membrane.Time.second()) |> Ratio.trunc()
  end

  @doc """
  Converts time from h264 time base (1/90_000 [s]) to membrane time base (1 [ns])
  """
  @spec to_membrane_time_base_truncated(number | Ratio.t()) :: integer | nil
  def to_membrane_time_base_truncated(@no_pts), do: nil

  def to_membrane_time_base_truncated(timestamp) do
    (timestamp * Membrane.Time.second() / @h264_time_base) |> Ratio.trunc()
  end
end
