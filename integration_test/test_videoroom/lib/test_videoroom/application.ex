defmodule TestVideoroom.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Phoenix.PubSub, name: TestVideoroom.PubSub},
      TestVideoroomWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: TestVideoroom.Supervisor]

    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    TestVideoroomWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
