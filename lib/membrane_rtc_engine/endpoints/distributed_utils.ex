defmodule Membrane.RTC.Engine.Endpoint.Distribution.Utils do
  @moduledoc false

  @spec distributed_sink_name(Node.t(), Node.t(), binary()) :: binary()
  def distributed_sink_name(sink_node, source_node, room_id),
    do: Enum.join([sink_node, source_node, room_id, :sink], ":")

  @spec distributed_source_name(Node.t(), Node.t(), binary()) :: binary()
  def distributed_source_name(sink_node, source_node, room_id),
    do: Enum.join([sink_node, source_node, room_id, :source], ":")

  @spec match_with_twin(binary()) :: {:ok, pid()} | {:error, any()}
  def match_with_twin(twin_name) do
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
