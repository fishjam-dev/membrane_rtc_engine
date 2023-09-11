defmodule Bundlex.Project.Store do
  @moduledoc false
  use Agent
  alias Bundlex.Project

  @spec start_link(any) :: Agent.on_start()
  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec get_project(application :: atom) :: Project.t() | nil
  def get_project(application) do
    Agent.get(__MODULE__, & &1[application])
  end

  @spec store_project(application :: atom, Project.t()) :: :ok
  def store_project(application, project) do
    Agent.update(__MODULE__, &Map.put(&1, application, project))
  end
end
