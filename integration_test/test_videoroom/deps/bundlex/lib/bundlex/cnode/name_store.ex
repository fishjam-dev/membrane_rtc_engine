defmodule Bundlex.CNode.NameStore do
  @moduledoc false
  # Responsibility of this module is to provide unique names for CNodes.
  # Name of each CNode consists of prefix, sequential number (referred to as
  # `seq_num`) and unique current application identifier (referred to as `self_id`).
  # In order not to create too many atoms, which are not garbage-collected,
  # names of dead CNodes are reused (they should be returned via `return_name/2`),
  # and therefore `creation` numbers are returned along with names to distinguish
  # between old and new CNode instances.

  use Agent

  @spec start_link(GenServer.options()) :: Agent.on_start()
  def start_link(opts \\ []) do
    Agent.start_link(
      fn -> %{seq_num: 0, q: Qex.new(), creations: %{}, self_id: SecureRandom.uuid()} end,
      opts ++ [name: __MODULE__]
    )
  end

  @spec get_self_name() :: name :: atom
  def get_self_name() do
    Agent.get(__MODULE__, fn %{self_id: self_id} -> :"bundlex_app_#{self_id}" end)
  end

  @spec get_name() :: {name :: atom, creation :: non_neg_integer}
  def get_name() do
    Agent.get_and_update(__MODULE__, fn state ->
      {name, q, seq_num} =
        case state.q |> Qex.pop() do
          {{:value, v}, q} ->
            {v, q, state.seq_num}

          {:empty, q} ->
            {:"bundlex_cnode_#{state.seq_num}_#{state.self_id}", q, state.seq_num + 1}
        end

      {{name, state.creations |> Map.get(name, 0)}, %{state | seq_num: seq_num, q: q}}
    end)
  end

  @spec return_name(name :: atom) :: :ok
  def return_name(name) do
    Agent.update(__MODULE__, fn state ->
      state
      |> Map.update!(:q, &Qex.push(&1, name))
      |> update_in([:creations, name], &((&1 || 0) + 1))
    end)
  end
end
