if Enum.all?(
     Membrane.RTC.Engine.MixProject.rtsp_endpoint_deps(),
     &Code.ensure_loaded?/1
   ) do
  defmodule Membrane.RTC.Engine.Endpoint.RTSP.PortAllocator do
    @moduledoc false
    use GenServer

    alias Membrane.RTC.Engine

    @spec start_link(opts :: Keyword.t()) :: GenServer.on_start()
    def start_link(opts) do
      GenServer.start_link(__MODULE__, opts)
    end

    @spec get_port(rtc_engine :: pid()) :: pos_integer() | nil
    def get_port(rtc_engine) do
      pid = Engine.get_rtsp_port_allocator(rtc_engine)
      GenServer.call(pid, :get_port)
    end

    @spec free_port(rtc_engine :: pid(), port_owner :: pid()) :: :ok
    def free_port(rtc_engine, port_owner) do
      pid = Engine.get_rtsp_port_allocator(rtc_engine)
      GenServer.cast(pid, {:free_port, port_owner})
    end

    @impl true
    def init(port_range: {range_start, range_end}) do
      ports = Enum.reduce(range_end..range_start//-1, [], fn x, acc -> [x | acc] end)
      {:ok, %{available_ports: ports, mapping: %{}}}
    end

    @impl true
    def handle_call(:get_port, {pid, _tag}, state) do
      {port, available_ports} =
        case state.available_ports do
          [port | rest] -> {port, rest}
          [] -> {nil, []}
        end

      mapping = if is_nil(port), do: state.mapping, else: Map.put(state.mapping, pid, port)

      {:reply, port, %{state | available_ports: available_ports, mapping: mapping}}
    end

    @impl true
    def handle_cast({:free_port, pid}, state) do
      {port, mapping} = Map.pop(state.mapping, pid)

      available_ports =
        if is_nil(port), do: state.available_ports, else: [port | state.available_ports]

      {:noreply, %{state | available_ports: available_ports, mapping: mapping}}
    end
  end
end
