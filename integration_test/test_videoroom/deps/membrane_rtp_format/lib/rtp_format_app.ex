defmodule Membrane.RTP.Format.App do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    :ok = Membrane.RTP.PayloadFormat.register_static_formats()
    Supervisor.start_link([], strategy: :one_for_one, name: __MODULE__)
  end
end
