defmodule WebRTCRoom do
  use Application

  @ip {127, 0, 0, 1}
  @port 5005

  @impl true
  def start(_type, _args) do
    children = [
      {Bandit, plug: __MODULE__.Router, ip: @ip, port: @port},
      __MODULE__.EngineHandler
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
