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

  @spec default_ffmpeg_filter(integer(), integer(), integer()) :: binary()
  def default_ffmpeg_filter(width, height, inputs_num)

  def default_ffmpeg_filter(width, height, 1) do
    "[0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1,pad=#{width}:#{height}:(ow-iw)/2:(oh-ih)/2"
  end

  def default_ffmpeg_filter(width, height, 2) do
    """
    [0:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[left];\
    [1:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];\
    [left][right]hstack\
    """
  end

  def default_ffmpeg_filter(width, height, 3) do
    """
    [0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];\
    [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];\
    [2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];\
    [lefttop][leftdown]vstack=inputs=2[left];\
    [left][right]hstack=inputs=2\
    """
  end

  def default_ffmpeg_filter(width, height, 4) do
    """
    [0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];\
    [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[righttop];\
    [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];\
    [3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[rightdown];\
    [lefttop][righttop]hstack=inputs=2[top];\
    [leftdown][rightdown]hstack=inputs=2[down];\
    [top][down]vstack=inputs=2\
    """
  end

  def default_ffmpeg_filter(width, height, 5) do
    rest = rem(width, 3)
    column_width = div(width, 3)

    """
    [0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];\
    [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];\
    [2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[right];\
    [3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];\
    [4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];\
    [lefttop][middletop]hstack=inputs=2[top];[leftdown][middledown]hstack=inputs=2[bottom];\
    [top][bottom]vstack=inputs=2[left];\
    [left][right]hstack=inputs=2\
    """
  end

  def default_ffmpeg_filter(width, height, 6) do
    rest = rem(width, 3)
    column_width = div(width, 3)

    """
    [0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];\
    [1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];\
    [2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[righttop];\
    [3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];\
    [4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];\
    [5:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[rightdown];\
    [lefttop][middletop][righttop]hstack=inputs=3[top];\
    [leftdown][middledown][rightdown]hstack=inputs=3[bottom];\
    [top][bottom]vstack=inputs=2\
    """
  end

  def default_ffmpeg_filter(_width, _height, inputs_num) do
    raise("No matching filter found for #{inputs_num} input(s)")
  end
end
