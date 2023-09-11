defmodule Membrane.RTP.H264.Plugin.App do
  @moduledoc false
  use Application
  alias Membrane.RTP.{H264, PayloadFormat}

  @impl true
  def start(_type, _args) do
    PayloadFormat.register(%PayloadFormat{
      encoding_name: :H264,
      payload_type: 96,
      payloader: H264.Payloader,
      depayloader: H264.Depayloader,
      keyframe_detector: &H264.Utils.is_keyframe/1
    })

    PayloadFormat.register_payload_type_mapping(96, :H264, 90_000)
    Supervisor.start_link([], strategy: :one_for_one, name: __MODULE__)
  end
end
