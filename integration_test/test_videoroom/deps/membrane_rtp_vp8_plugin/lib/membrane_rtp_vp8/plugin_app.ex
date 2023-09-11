defmodule Membrane.RTP.VP8.Plugin.App do
  @moduledoc false
  use Application
  alias Membrane.RTP.{PayloadFormat, VP8}

  @impl true
  def start(_type, _args) do
    PayloadFormat.register(%PayloadFormat{
      encoding_name: :VP8,
      payload_type: 98,
      depayloader: VP8.Depayloader,
      payloader: VP8.Payloader,
      keyframe_detector: &VP8.Utils.is_keyframe/1,
      frame_detector: &VP8.Utils.is_new_frame/1
    })

    PayloadFormat.register_payload_type_mapping(98, :VP8, 90_000)
    Supervisor.start_link([], strategy: :one_for_one, name: __MODULE__)
  end
end
