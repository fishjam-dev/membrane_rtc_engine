defmodule Membrane.OpenTelemetry.Application do
  @moduledoc false
  use Application

  alias Membrane.OpenTelemetry.ETSUtils

  @impl true
  def start(_type, _args) do
    :ok = ETSUtils.setup_ets_table()

    children = []
    opts = [strategy: :one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    ETSUtils.delete_ets_table()
    :ok
  end
end
