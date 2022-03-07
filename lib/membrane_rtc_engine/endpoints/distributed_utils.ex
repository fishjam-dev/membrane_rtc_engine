defmodule Membrane.RTC.Engine.Endpoint.Distribution.Utils do
  @moduledoc false

  @spec distributed_sink_name(Node.t(), Node.t(), binary()) :: binary()
  def distributed_sink_name(sink_node, source_node, room_id),
    do: Enum.join([sink_node, source_node, room_id, :sink], ":")

  @spec distributed_source_name(Node.t(), Node.t(), binary()) :: binary()
  def distributed_source_name(sink_node, source_node, room_id),
    do: Enum.join([sink_node, source_node, room_id, :source], ":")

  @spec match_with_twin(binary(), boolean()) ::
          {:ok, :gen_udp.socket(), pid()}
          | {:ok, :gen_udp.socket(), pid(), :inet.port_number()}
          | {:error, any()}
  def match_with_twin(twin_name, receive_twin_port? \\ true) do
    case get_twin_pid(twin_name) do
      {:ok, twin} ->
        {:ok, socket} = :gen_udp.open(0)
        {:ok, port} = :inet.port(socket)
        send(twin, {:twin_port, port})

        if receive_twin_port? do
          receive do
            {:twin_port, twin_port} ->
              {:ok, socket, twin, twin_port}
          after
            5_000 ->
              :gen_udp.close(socket)
              {:error, :twin_not_responding}
          end
        else
          {:ok, socket, twin}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp get_twin_pid(twin_name) do
    case :global.whereis_name(twin_name) do
      twin when is_pid(twin) ->
        send(twin, {:twin, self()})
        {:ok, twin}

      :undefined ->
        receive do
          {:twin, twin} ->
            {:ok, twin}
        after
          5_000 ->
            {:error, :twin_not_responding}
        end
    end
  end
end
