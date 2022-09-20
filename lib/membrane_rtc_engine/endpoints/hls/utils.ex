defmodule Membrane.RTC.Engine.Endpoint.HLS.Utils do
  @moduledoc false

  @audio_codecs [:AAC, :OPUS]
  @video_codecs [:H264]

  # 3rd clause for non-existant codecs?
  def get_track_type(track) when track.encoding in @audio_codecs, do: :audio
  def get_track_type(track) when track.encoding in @video_codecs, do: :video
  def get_track_type(_track), do: raise "Unsupported codec"
end
