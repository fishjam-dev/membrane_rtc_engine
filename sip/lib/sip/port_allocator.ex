defmodule Membrane.RTC.Engine.Endpoint.SIP.PortAllocator do
  @moduledoc false

  use GenServer
  require Logger

  @default_port_range {21_000, 21_100}

  @spec start_link(term()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec get_port() :: {:ok, pos_integer()} | {:error, :no_available_port}
  def get_port() do
    GenServer.call(__MODULE__, :get_port)
  end

  @spec free_ports(pid()) :: :ok
  def free_ports(port_owner) do
    GenServer.cast(__MODULE__, {:free_ports, port_owner})
  end

  @impl true
  def init(_opts) do
    {from, to} = Application.get_env(:membrane_rtc_engine_sip, :port_range, @default_port_range)

    state = %{
      available: from..to |> Enum.to_list() |> :queue.from_list(),
      in_use: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_port, {pid, _tag} = from, state) do
    case :queue.out(state.available) do
      {{:value, port}, available} ->
        state = %{state | available: available}

        case :gen_udp.open(port) do
          {:ok, socket} ->
            :ok = :gen_udp.close(socket)

            state = %{state | in_use: Map.update(state.in_use, pid, [port], &[port | &1])}

            {:reply, {:ok, port}, state}

          {:error, reason} ->
            Logger.warning("Opening port #{port} failed with reason: #{reason}")

            handle_call(:get_port, from, state)
        end

      {:empty, _available} ->
        {:reply, {:error, :no_available_port}, state}
    end
  end

  @impl true
  def handle_cast({:free_ports, pid}, state) do
    {ports, in_use} = Map.pop(state.in_use, pid, [])

    available =
      Enum.reduce(ports, state.available, fn elem, acc ->
        :queue.in(elem, acc)
      end)

    state = %{state | available: available, in_use: in_use}

    {:noreply, state}
  end
end
