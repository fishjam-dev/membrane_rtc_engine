defmodule Membrane.WebRTC.Metrics do
  @moduledoc """
  Defines a list of metrics that can be aggregated based on events from `membrane_webrtc_plugin`.
  """

  alias Telemetry.Metrics

  @spec metrics() :: [Metrics.t()]
  def metrics(),
    do: [
      Metrics.last_value(
        "sdp.offer",
        event_name: [Membrane.WebRTC, :sdp, :offer],
        measurement: :sdp
      ),
      Metrics.last_value(
        "sdp.answer",
        event_name: [Membrane.WebRTC, :sdp, :answer],
        measurement: :sdp
      )
    ]
end
