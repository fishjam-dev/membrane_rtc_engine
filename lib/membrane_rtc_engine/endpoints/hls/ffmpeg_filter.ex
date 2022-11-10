if Code.ensure_loaded?(VideoMixer) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.FFmpegFilter do
    @moduledoc """
    Returns FFmpeg filtergraph that is used to create compositor output.
    https://ffmpeg.org/ffmpeg-filters.html#toc-Filtergraph-description

    Used ffmpeg options:
      force_original_aspect_ratio=decrease - the output video dimensions will automatically be decreased if needed.
      setsar - sets the Sample (aka Pixel) Aspect Ratio for the filter output video.
      iw/ih - width/height of the input video.
      ow/oh - the output (cropped) width and height.
      pad - add paddings to the input image, and place the original input at the provided x, y coordinates.
      crop - the input video to given dimensions.
      vstack - stack input videos vertically.
      hstack - stack input videos horizontally.
    """

    alias VideoMixer.FrameSpec

    @spec default_ffmpeg_filter(FrameSpec.t(), [FrameSpec.t()], any()) :: binary()
    def default_ffmpeg_filter(output, inputs, _state),
      do: do_default_ffmpeg_filter(output, Enum.count(inputs))

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 1) do
      {"[0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1,pad=#{width}:#{height}:(ow-iw)/2:(oh-ih)/2",
       0..0}
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 2) do
      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[left];\
      [1:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[right];\
      [left][right]overlay=shortest=1
      """
      |> then(&{&1, 0..1})
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 3) do
      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[base];\
      [1:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[left];\
      [2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];\
      [left][right]hstack[left+right];\
      [base][left+right]overlay=shortest=1
      """
      |> then(&{&1, 0..2})
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 4) do
      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[base];\
      [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];\
      [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];\
      [3:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];\
      [lefttop][leftdown]vstack=inputs=2[left];\
      [left][right]hstack=inputs=2[all];\
      [base][all]overlay=shortest=1
      """
      |> then(&{&1, 0..3})
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 5) do
      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[base];\
      [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];\
      [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[righttop];\
      [3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];\
      [4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[rightdown];\
      [lefttop][righttop]hstack=inputs=2[top];\
      [leftdown][rightdown]hstack=inputs=2[down];\
      [top][down]vstack=inputs=2[all];\
      [base][all]overlay=shortest=1
      """
      |> then(&{&1, 0..4})
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 6) do
      rest = rem(width, 3)
      column_width = div(width, 3)

      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[base];\
      [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];\
      [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];\
      [3:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[right];\
      [4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];\
      [5:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];\
      [lefttop][middletop]hstack=inputs=2[top];[leftdown][middledown]hstack=inputs=2[bottom];\
      [top][bottom]vstack=inputs=2[left];\
      [left][right]hstack=inputs=2;\
      [base][all]overlay=shortest=1
      """
      |> then(&{&1, 0..5})
    end

    defp do_default_ffmpeg_filter(%FrameSpec{width: width, height: height}, 7) do
      rest = rem(width, 3)

      column_width = div(width, 3)

      """
      [0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1[base];\
      [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];\
      [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];\
      [3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[righttop];\
      [4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];\
      [5:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];\
      [6:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[rightdown];\
      [lefttop][middletop][righttop]hstack=inputs=3[top];\
      [leftdown][middledown][rightdown]hstack=inputs=3[bottom];\
      [top][bottom]vstack=inputs=2;\
      [base][all]overlay=shortest=1
      """
      |> then(&{&1, 0..6})
    end

    defp do_default_ffmpeg_filter(frame_spec, number) do
      raise("No matching filter found for #{number} input(s) and frame_spec: #{frame_spec}")
    end
  end
end
