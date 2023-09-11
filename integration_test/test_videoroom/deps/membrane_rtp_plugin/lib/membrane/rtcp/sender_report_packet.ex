defmodule Membrane.RTCP.SenderReportPacket do
  @moduledoc """
  Parses and constructs RTCP Sender Report defined in
  [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.4.1)
  """

  @behaviour Membrane.RTCP.Packet

  alias Membrane.RTCP.ReportPacketBlock
  alias Membrane.RTP

  defstruct [:ssrc, :reports, :sender_info]

  @type sender_info_t :: %{
          wallclock_timestamp: Membrane.Time.t(),
          rtp_timestamp: non_neg_integer(),
          sender_packet_count: non_neg_integer(),
          sender_octet_count: non_neg_integer()
        }

  @type t :: %__MODULE__{
          ssrc: RTP.ssrc_t(),
          reports: [ReportPacketBlock.t()],
          sender_info: sender_info_t()
        }

  @impl true
  def encode(report) do
    sender_info = encode_sender_info(report.sender_info)
    blocks = report.reports |> Enum.map_join(&ReportPacketBlock.encode/1)

    reports_count = report.reports |> length()

    body = <<report.ssrc::32>> <> sender_info <> blocks

    {body, reports_count}
  end

  defp encode_sender_info(sender_info) do
    ntp_timestamp = Membrane.Time.to_ntp_timestamp(sender_info.wallclock_timestamp)

    <<ntp_timestamp::bitstring-size(64), sender_info.rtp_timestamp::32,
      sender_info.sender_packet_count::32, sender_info.sender_octet_count::32>>
  end

  @impl true
  def decode(
        <<ssrc::32, ntp_timestamp::bitstring-size(64), rtp_time::32, packet_count::32,
          octet_count::32, blocks::binary>>,
        reports_count
      ) do
    wallclock_ts = Membrane.Time.from_ntp_timestamp(ntp_timestamp)

    sender_info = %{
      wallclock_timestamp: wallclock_ts,
      rtp_timestamp: rtp_time,
      sender_packet_count: packet_count,
      sender_octet_count: octet_count
    }

    with {:ok, reports} <- ReportPacketBlock.decode(blocks),
         true <- reports_count == length(reports) do
      {:ok, %__MODULE__{ssrc: ssrc, reports: reports, sender_info: sender_info}}
    else
      false -> {:error, :invalid_reports_count}
      err -> err
    end
  end

  def decode(_packet, _reports_count) do
    {:error, :sr_too_short}
  end
end
