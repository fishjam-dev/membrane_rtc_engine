defmodule Membrane.RTC.Engine.Endpoint.HLS.FFmpegFilter do
  @moduledoc false

  @spec default_ffmpeg_filter(integer(), integer(), integer()) :: binary()
  def default_ffmpeg_filter(width, height, 1) do
    "[0:v]scale=#{width}:#{height}:force_original_aspect_ratio=decrease,setsar=1/1,pad=#{width}:#{height}:(ow-iw)/2:(oh-ih)/2"
  end

  def default_ffmpeg_filter(width, height, 2) do
    "[0:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[left];"
    |> Kernel.<>(
      "[1:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];"
    )
    |> Kernel.<>("[left][right]hstack")
  end

  def default_ffmpeg_filter(width, height, 3) do
    "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];"
    |> Kernel.<>(
      "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];"
    )
    |> Kernel.<>(
      "[2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[right];"
    )
    |> Kernel.<>("[lefttop][leftdown]vstack=inputs=2[left];")
    |> Kernel.<>("[left][right]hstack=inputs=2")
  end

  def default_ffmpeg_filter(width, height, 4) do
    "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[lefttop];"
    |> Kernel.<>(
      "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[righttop];"
    )
    |> Kernel.<>(
      "[2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[leftdown];"
    )
    |> Kernel.<>(
      "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{width}/2:ih,setsar=1[rightdown];"
    )
    |> Kernel.<>("[lefttop][righttop]hstack=inputs=2[top];")
    |> Kernel.<>("[leftdown][rightdown]hstack=inputs=2[down];")
    |> Kernel.<>("[top][down]vstack=inputs=2")
  end

  def default_ffmpeg_filter(width, height, 5) do
    rest = rem(width, 3)
    column_width = div(width, 3)

    "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];"
    |> Kernel.<>(
      "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];"
    )
    |> Kernel.<>(
      "[2:v]scale=-1:#{height}:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[right];"
    )
    |> Kernel.<>(
      "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];"
    )
    |> Kernel.<>(
      "[4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];"
    )
    |> Kernel.<>(
      "[lefttop][middletop]hstack=inputs=2[top];[leftdown][middledown]hstack=inputs=2[bottom];"
    )
    |> Kernel.<>("[top][bottom]vstack=inputs=2[left];")
    |> Kernel.<>("[left][right]hstack=inputs=2")
  end

  def default_ffmpeg_filter(width, height, 6) do
    rest = rem(width, 3)
    column_width = div(width, 3)

    "[0:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[lefttop];"
    |> Kernel.<>(
      "[1:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middletop];"
    )
    |> Kernel.<>(
      "[2:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[righttop];"
    )
    |> Kernel.<>(
      "[3:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[leftdown];"
    )
    |> Kernel.<>(
      "[4:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}:ih,setsar=1[middledown];"
    )
    |> Kernel.<>(
      "[5:v]scale=-1:#{height}/2:force_original_aspect_ratio=decrease,crop=#{column_width}+#{rest}:ih,setsar=1[rightdown];"
    )
    |> Kernel.<>("[lefttop][middletop][righttop]hstack=inputs=3[top];")
    |> Kernel.<>("[leftdown][middledown][rightdown]hstack=inputs=3[bottom];")
    |> Kernel.<>("[top][bottom]vstack=inputs=2")
  end

  def default_ffmpeg_filter(_width, _height, n) do
    raise("No matching filter found for #{n} input(s)")
  end
end
