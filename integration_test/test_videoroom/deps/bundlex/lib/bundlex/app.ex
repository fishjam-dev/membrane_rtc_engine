defmodule Bundlex.App do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [Bundlex.CNode.NameStore, Bundlex.Project.Store]
    opts = [strategy: :one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end
end
