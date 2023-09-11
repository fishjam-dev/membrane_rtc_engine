defmodule Membrane.RTP.StreamSendBin do
  @moduledoc """
  Bin payloading and serializing media stream to RTP.
  """
  use Membrane.Bin
  alias Membrane.RTP

  def_input_pad :input, demand_unit: :buffers, accepted_format: _any

  def_input_pad :rtcp_input,
    availability: :on_request,
    demand_unit: :buffers,
    accepted_format: _any

  def_output_pad :output, accepted_format: _any, demand_unit: :buffers

  def_output_pad :rtcp_output,
    availability: :on_request,
    accepted_format: _any,
    demand_unit: :buffers

  def_options payloader: [default: nil, spec: module],
              payload_type: [spec: RTP.payload_type_t()],
              ssrc: [spec: RTP.ssrc_t()],
              clock_rate: [spec: RTP.clock_rate_t()],
              rtcp_report_interval: [spec: Membrane.Time.t() | nil],
              rtp_extension_mapping: [
                default: nil,
                spec: RTP.SessionBin.rtp_extension_mapping_t()
              ],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: []
              ]

  @impl true
  def handle_init(_ctx, opts) do
    use_payloader = !is_nil(opts.payloader)

    add_payloader_bin =
      &child(&1, :payloader, %RTP.PayloaderBin{
        payloader: opts.payloader,
        ssrc: opts.ssrc,
        clock_rate: opts.clock_rate,
        payload_type: opts.payload_type
      })

    structure =
      bin_input()
      |> then(if use_payloader, do: add_payloader_bin, else: & &1)
      |> child(:packet_tracker, %RTP.OutboundTrackingSerializer{
        ssrc: opts.ssrc,
        payload_type: opts.payload_type,
        clock_rate: opts.clock_rate,
        extension_mapping: opts.rtp_extension_mapping || %{},
        telemetry_label: opts.telemetry_label
      })
      |> bin_output()

    {[spec: structure], %{ssrc: opts.ssrc, rtcp_report_interval: opts.rtcp_report_interval}}
  end

  @impl true
  def handle_playing(_context, %{rtcp_report_interval: nil} = state) do
    {[], state}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[start_timer: {:report_timer, state.rtcp_report_interval}], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_output, _id) = pad, _ctx, state) do
    structure =
      get_child(:packet_tracker)
      |> via_out(:rtcp_output)
      |> bin_output(pad)

    {[spec: structure], state}
  end

  @impl true
  def handle_pad_added(Pad.ref(:rtcp_input, _id) = pad, _ctx, state) do
    structure = [
      bin_input(pad)
      |> via_in(:rtcp_input)
      |> get_child(:packet_tracker)
    ]

    {[spec: structure], state}
  end

  @impl true
  def handle_tick(:report_timer, _ctx, state) do
    {[notify_child: {:packet_tracker, :send_stats}], state}
  end
end
