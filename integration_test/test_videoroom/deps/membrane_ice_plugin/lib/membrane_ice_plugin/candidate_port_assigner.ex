defmodule Membrane.ICE.CandidatePortAssigner do
  @moduledoc false

  @min_port 40_000
  @max_port 65_535

  @spec start_link() :: {:ok, pid()} | {:error, term()}
  def start_link() do
    Registry.start_link(name: __MODULE__, keys: :unique)
  end

  @spec assign_candidate_port() ::
          {:ok, number()} | {:error, :no_free_candidate_port}
  def assign_candidate_port() do
    Enum.random(@min_port..@max_port)
    |> do_assign_candidate_port(0)
  end

  @spec get_candidate_port_owner(number()) ::
          {:ok, pid()} | {:error, :candidate_port_owner_not_alive | :port_number_is_not_valid}

  def get_candidate_port_owner(port) when port in @min_port..@max_port do
    case Registry.lookup(__MODULE__, port) do
      [{pid, nil}] -> {:ok, pid}
      [] -> {:error, :candidate_port_owner_not_alive}
    end
  end

  def get_candidate_port_owner(_port),
    do: {:error, :port_number_is_not_valid}

  defp do_assign_candidate_port(current_port, counter) do
    case Registry.register(__MODULE__, current_port, nil) do
      {:ok, _pid} ->
        {:ok, current_port}

      {:error, {:already_registered, _pid}} ->
        cond do
          counter == @max_port - @min_port ->
            {:error, :no_free_candidate_port}

          current_port == @max_port ->
            do_assign_candidate_port(@min_port, counter + 1)

          true ->
            do_assign_candidate_port(current_port + 1, counter + 1)
        end
    end
  end
end
