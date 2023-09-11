defmodule Membrane.RTCP.ReceiverReportPacket do
  @moduledoc """
  Parses and constructs RTCP Receiver Report defined in
  [RFC3550](https://tools.ietf.org/html/rfc3550#section-6.4.2)
  """

  @behaviour Membrane.RTCP.Packet

  alias Membrane.RTCP.ReportPacketBlock

  defstruct [:ssrc, :reports]

  @type t :: %__MODULE__{
          ssrc: non_neg_integer(),
          reports: [ReportPacketBlock.t()]
        }

  @impl true
  def encode(report) do
    blocks = report.reports |> Enum.map_join(&ReportPacketBlock.encode/1)
    body = <<report.ssrc::32>> <> blocks
    reports_count = report.reports |> length()
    {body, reports_count}
  end

  @impl true
  def decode(<<ssrc::32, blocks::binary>>, reports_count) do
    with {:ok, reports} <- ReportPacketBlock.decode(blocks),
         true <- reports_count == length(reports) do
      {:ok, %__MODULE__{ssrc: ssrc, reports: reports}}
    else
      false -> {:error, :invalid_reports_count}
      err -> err
    end
  end
end
