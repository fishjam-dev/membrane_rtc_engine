defmodule Membrane.RTC.Engine.PushOutputTee do
  @moduledoc """
  Element forwarding packets to multiple push outputs.
  """
  use Membrane.Filter

  def_options codec: [
                type: :atom,
                spec: [:H264 | :VP8 | :OPUS],
                description: "Codec of track #{inspect(__MODULE__)} will forward."
              ]

  def_input_pad :input,
    availability: :always,
    mode: :pull,
    demand_mode: :auto,
    caps: :any

  def_output_pad :output,
    availability: :on_request,
    mode: :push,
    caps: :any

  @impl true
  def handle_init(opts) do
    {:ok, %{codec: opts.codec}}
  end

  @impl true
  def handle_process(:input, %Membrane.Buffer{} = buffer, _ctx, state) do
    Membrane.RTC.Utils.emit_telemetry_event_with_packet_mesaurments(
      buffer.payload,
      buffer.metadata.rtp.ssrc,
      state.codec
    )

    {{:ok, forward: buffer}, state}
  end
end
