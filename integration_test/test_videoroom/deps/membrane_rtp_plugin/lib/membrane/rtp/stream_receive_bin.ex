defmodule Membrane.RTP.StreamReceiveBin do
  @moduledoc """
  This bin gets a parsed RTP stream on input and outputs raw media stream.

  Its responsibility is to depayload the RTP stream and compensate the
  jitter.
  """

  use Membrane.Bin

  alias Membrane.{RTCP, RTP, SRTP}

  def_options srtp_policies: [
                spec: [ExLibSRTP.Policy.t()],
                default: []
              ],
              secure?: [
                spec: boolean(),
                default: false
              ],
              extensions: [
                spec: [RTP.SessionBin.extension_t()],
                default: []
              ],
              clock_rate: [
                spec: RTP.clock_rate_t()
              ],
              depayloader: [spec: module() | nil],
              local_ssrc: [spec: RTP.ssrc_t()],
              remote_ssrc: [spec: RTP.ssrc_t()],
              rtcp_report_interval: [spec: Membrane.Time.t() | nil],
              telemetry_label: [
                spec: [{atom(), any()}],
                default: []
              ]

  def_input_pad :input, accepted_format: _any, demand_unit: :buffers
  def_output_pad :output, accepted_format: _any, demand_unit: :buffers

  @impl true
  def handle_init(_ctx, opts) do
    if opts.secure? and not Code.ensure_loaded?(ExLibSRTP),
      do: raise("Optional dependency :ex_libsrtp is required when using secure? option")

    add_decryptor =
      &child(&1, :decryptor, struct(SRTP.Decryptor, %{policies: opts.srtp_policies}))

    add_depayloader_bin =
      &child(&1, :depayloader, %RTP.DepayloaderBin{
        depayloader: opts.depayloader,
        clock_rate: opts.clock_rate
      })

    structure =
      bin_input()
      |> add_extensions(opts.extensions)
      |> child(:rtcp_receiver, %RTCP.Receiver{
        local_ssrc: opts.local_ssrc,
        remote_ssrc: opts.remote_ssrc,
        report_interval: opts.rtcp_report_interval,
        telemetry_label: opts.telemetry_label
      })
      |> child(:packet_tracker, %RTP.InboundPacketTracker{
        clock_rate: opts.clock_rate,
        repair_sequence_numbers?: true
      })
      |> then(if opts.secure?, do: add_decryptor, else: & &1)
      |> then(if opts.depayloader, do: add_depayloader_bin, else: & &1)
      |> bin_output()

    {[spec: structure], %{}}
  end

  defp add_extensions(link_builder, extensions) do
    Enum.reduce(extensions, link_builder, fn {extension_name, extension}, builder ->
      builder |> child(extension_name, extension)
    end)
  end
end
