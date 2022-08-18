defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackAdapter do
  @moduledoc false

  # Adapter:
  # * generates probe packets on request from the
  # outside
  # * switches between simulcast layers on request from
  # the outside
  # * adjusts RTP packets (sequence numbers, timestamps,
  # VP8 payload headers, etc.)

  use Membrane.Filter

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this adapter will maintain"
              ]

  def_input_pad :input,
    availability: :always,
    mode: :push,
    caps: Membrane.RTP,
    options: [
      telemetry_label: [
        spec: Membrane.TelemetryMetrics.label(),
        default: [],
        description: "Label passed to Membrane.TelemetryMetrics functions"
      ]
    ]

  def_output_pad :output,
    availability: :always,
    mode: :push,
    caps: Membrane.RTP

  @impl true
  def handle_init(%__MODULE__{track: track}) do
    state = %{track: track}
    {:ok, state}
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    actions = [buffer: {:output, buffer}]
    {{:ok, actions}, state}
  end
end
