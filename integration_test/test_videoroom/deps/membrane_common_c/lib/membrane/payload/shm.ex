defmodule Membrane.Payload.Shm do
  @moduledoc """
  Wrapper over `Shmex` module, implementing `Membrane.Payload.Behaviour`
  """
  @behaviour Membrane.Payload.Behaviour

  @type t :: Shmex.t()

  @impl true
  defdelegate empty(), to: Shmex

  @impl true
  defdelegate new(data), to: Shmex
end

defimpl Membrane.Payload, for: Shmex do
  @impl true
  def size(%Shmex{size: size}) do
    size
  end

  @impl true
  def split_at(%Shmex{size: size} = shm, at_pos) when 0 < at_pos and at_pos < size do
    {:ok, payloads} = Shmex.Native.split_at(shm, at_pos)
    payloads
  end

  @impl true
  def concat(left, right) do
    {:ok, res} = Shmex.Native.append(left, right)
    res
  end

  @impl true
  def drop(payload, n) do
    {:ok, new_payload} = Shmex.Native.trim(payload, n)
    new_payload
  end

  @impl true
  defdelegate to_binary(payload), to: Shmex

  @impl true
  def module(_), do: Membrane.Payload.Shm
end
