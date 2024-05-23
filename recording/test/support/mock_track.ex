defmodule MockTrack do
  @moduledoc false

  alias ExSDP.Attribute.FMTP
  alias Membrane.RTC.Engine.Track
  alias Membrane.RTCP.SenderReportPacket

  @spec create_track(:audio | :video) :: Track.t()
  def create_track(type) do
    id = UUID.uuid1()

    clock_rate =
      case type do
        :audio -> 48_000
        :video -> 90_000
      end

    %Track{
      type: type,
      stream_id: "stream_id",
      id: "#{id}_#{type}",
      origin: "origin",
      fmtp: %FMTP{pt: nil},
      encoding: :h264,
      variants: [:high],
      clock_rate: clock_rate,
      active?: true,
      metadata: %{},
      ctx: %{},
      framerate: nil,
      disabled_variants: []
    }
  end

  @spec create_rtcp_report(non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          SenderReportPacket.t()
  def create_rtcp_report(rtp_timestamp, wallclock_timestamp, clock_rate) do
    %SenderReportPacket{
      ssrc: 1,
      reports: [],
      sender_info: %{
        wallclock_timestamp: wallclock_timestamp,
        rtp_timestamp: rtp_timestamp + clock_rate,
        sender_packet_count: 0,
        sender_octet_count: 0
      }
    }
  end
end
