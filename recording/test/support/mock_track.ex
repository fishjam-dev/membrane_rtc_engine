defmodule MockTrack do
  @moduledoc false

  alias ExSDP.Attribute.FMTP
  alias Membrane.RTC.Engine.Track

  @spec create_track(:audio | :video) :: Track.t()
  def create_track(type) do
    %Track{
      type: type,
      stream_id: "stream_id",
      id: "id",
      origin: "origin",
      fmtp: %FMTP{pt: nil},
      encoding: :h264,
      variants: [:high],
      clock_rate: 90_000,
      active?: true,
      metadata: %{},
      ctx: %{},
      framerate: nil
    }
  end
end
