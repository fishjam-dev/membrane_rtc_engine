defmodule Membrane.ICE.TURNManager do
  @moduledoc false

  require Membrane.Logger

  alias Membrane.ICE

  @spec start_link() :: {:ok, pid()} | {:error, term()}
  def start_link() do
    Agent.start_link(fn -> [] end, name: __MODULE__)
  end

  @spec ensure_tcp_turn_launched(ICE.Endpoint.integrated_turn_options_t(), Keyword.t()) :: :ok
  def ensure_tcp_turn_launched(turn_options, opts \\ []),
    do: do_ensure_turn_launched(:tcp, turn_options, opts)

  @spec ensure_tls_turn_launched(ICE.Endpoint.integrated_turn_options_t(), Keyword.t()) ::
          :ok | {:error, :lack_of_cert_file_turn_option}
  def ensure_tls_turn_launched(turn_options, opts \\ []) do
    if turn_options[:cert_file] do
      do_ensure_turn_launched(:tls, turn_options, opts)
    else
      {:error, :lack_of_cert_file_turn_option}
    end
  end

  @spec get_launched_turn_servers() :: [any()]
  def get_launched_turn_servers() do
    Agent.get(__MODULE__, & &1)
  end

  @spec stop_launched_turn_servers() :: :ok
  def stop_launched_turn_servers() do
    Agent.update(__MODULE__, fn turns ->
      Enum.each(turns, &ICE.Utils.stop_integrated_turn/1)
      []
    end)
  end

  defp do_ensure_turn_launched(transport, turn_options, opts) do
    Agent.update(__MODULE__, fn turns ->
      if Enum.any?(turns, &(&1.relay_type == transport)) do
        turns
      else
        new_turns = ICE.Utils.start_integrated_turn_servers([transport], turn_options, opts)

        new_turns ++ turns
      end
    end)
  end
end
