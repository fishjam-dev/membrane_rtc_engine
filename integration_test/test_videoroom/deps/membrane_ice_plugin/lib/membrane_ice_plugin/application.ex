defmodule Membrane.ICE.Application do
  @moduledoc false

  use Application

  alias Membrane.ICE.{
    CandidatePortAssigner,
    TURNManager
  }

  @impl true
  def start(_type, _args) do
    children = [
      %{
        id: CandidatePortAssigner,
        start: {CandidatePortAssigner, :start_link, []}
      },
      %{
        id: TURNManager,
        start: {TURNManager, :start_link, []}
      }
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end
end
