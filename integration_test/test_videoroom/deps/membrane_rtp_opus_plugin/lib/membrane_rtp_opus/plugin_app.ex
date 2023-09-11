defmodule Membrane.RTP.Opus.Plugin.App do
  @moduledoc false
  use Application
  alias Membrane.RTP.{Opus, PayloadFormat}

  @spec start(atom, list) ::
          {:ok, pid} | {:error, {:already_started, pid} | {:shutdown, term} | term}
  def start(_type, _args) do
    PayloadFormat.register(%PayloadFormat{
      encoding_name: :OPUS,
      payload_type: 120,
      payloader: Opus.Payloader,
      depayloader: Opus.Depayloader,
      frame_detector: fn _payload -> true end
    })

    PayloadFormat.register_payload_type_mapping(120, :OPUS, 48_000)
    Supervisor.start_link([], strategy: :one_for_one, name: __MODULE__)
  end
end
