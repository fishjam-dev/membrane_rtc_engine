defmodule Membrane.RTC.Engine.Endpoint.Recording.Application do
  @moduledoc false
  use Application

  alias Membrane.RTC.Engine.Endpoint.Recording

  @impl true
  def start(_start_type, _start_args) do
    children = [
      {Task.Supervisor, name: Recording.TaskSupervisor, strategy: :one_for_one}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
