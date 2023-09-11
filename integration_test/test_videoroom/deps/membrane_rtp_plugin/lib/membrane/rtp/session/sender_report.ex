defmodule Membrane.RTP.Session.SenderReport do
  @moduledoc false
  require Bitwise
  require Membrane.Logger
  alias Membrane.{RTCP, RTP, Time}

  @timestamp_limit Bitwise.bsl(1, 32)

  @typedoc """
  Mapping from `ssrc` to its sender statistics.
  """
  @type sender_stats_t :: %{
          non_neg_integer() => %{
            clock_rate: non_neg_integer(),
            timestamp: non_neg_integer(),
            rtp_timestamp: non_neg_integer(),
            sender_packet_count: non_neg_integer(),
            sender_octet_count: non_neg_integer()
          }
        }

  defmodule Data do
    @moduledoc false
    @type t :: %__MODULE__{
            senders_ssrcs: MapSet.t(RTP.ssrc_t()),
            stats: %{
              RTP.ssrc_t() => RTP.Serializer.Stats.t()
            }
          }

    defstruct senders_ssrcs: MapSet.new(),
              stats: %{}
  end

  @type maybe_report_t :: {:report, RTCP.Packet.t()} | :no_report

  @spec init_report(ssrcs :: MapSet.t(RTP.ssrc_t()), report_data :: Data.t()) ::
          {MapSet.t(RTP.ssrc_t()), Data.t()}
  def init_report(ssrcs, %Data{senders_ssrcs: senders_ssrcs} = report_data)
      when senders_ssrcs == %MapSet{} do
    senders_stats =
      report_data.stats |> Bunch.KVEnum.filter_by_keys(&MapSet.member?(ssrcs, &1)) |> Map.new()

    report_data = %{
      report_data
      | senders_ssrcs: ssrcs,
        stats: senders_stats
    }

    {ssrcs, report_data}
  end

  @spec flush_report(Data.t()) :: {maybe_report_t(), Data.t()}
  def flush_report(report_data) do
    if Enum.empty?(report_data.senders_ssrcs) do
      {:no_report, report_data}
    else
      Membrane.Logger.debug(
        "Not received sender stats from ssrcs: #{Enum.join(report_data.senders_ssrcs, ", ")}"
      )

      case generate_report(report_data.stats) do
        [] ->
          {:no_report, report_data}

        sender_reports ->
          {{:report, sender_reports}, %{report_data | senders_ssrcs: MapSet.new(), stats: %{}}}
      end
    end
  end

  @spec handle_stats(RTP.Serializer.Stats.t(), RTP.ssrc_t(), Data.t()) ::
          {maybe_report_t(), Data.t()}
  def handle_stats(stats, sender_ssrc, data) do
    senders_ssrcs = MapSet.delete(data.senders_ssrcs, sender_ssrc)

    data = %{data | stats: Map.put(data.stats, sender_ssrc, stats), senders_ssrcs: senders_ssrcs}

    if Enum.empty?(senders_ssrcs) do
      case generate_report(data.stats) do
        [] -> {:no_report, data}
        sender_reports -> {{:report, sender_reports}, data}
      end
    else
      {:no_report, data}
    end
  end

  @spec generate_report(sender_stats_t()) :: [RTCP.SenderReportPacket.t()]
  def generate_report(stats) do
    stats
    |> Enum.filter(fn {_k, v} -> v != :no_stats end)
    |> Enum.flat_map(fn {sender_ssrc, sender_stats} ->
      generate_sender_report(sender_ssrc, sender_stats)
    end)
  end

  defp generate_sender_report(sender_ssrc, sender_stats) do
    timestamp = Time.vm_time()

    rtp_offset =
      (timestamp - sender_stats.timestamp)
      |> Ratio.mult(sender_stats.clock_rate)
      |> Time.round_to_seconds()

    rtp_timestamp = rem(sender_stats.rtp_timestamp + rtp_offset, @timestamp_limit)

    sender_info = %{
      wallclock_timestamp: timestamp,
      rtp_timestamp: rtp_timestamp,
      sender_packet_count: sender_stats.sender_packet_count,
      sender_octet_count: sender_stats.sender_octet_count
    }

    [%RTCP.SenderReportPacket{ssrc: sender_ssrc, reports: [], sender_info: sender_info}]
  end
end
