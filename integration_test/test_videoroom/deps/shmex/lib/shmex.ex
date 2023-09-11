defmodule Shmex do
  @moduledoc """
  This module allows using data placed in POSIX shared memory on POSIX
  compliant systems.

  Defines a struct representing the actual shared memory object. The struct
  should not be modified, and should always be passed around as a whole - see
  `t:#{inspect(__MODULE__)}.t/0`
  """
  alias __MODULE__.Native

  @typedoc """
  Struct describing data kept in shared memory. Should not be modified
  and should always be passed around as a whole

  ...including passing to the native code - there are functions in `:shmex_lib`
  (a native library exported via Bundlex) that will allow to transform Elixir
  struct into a C struct and then access the shared memory from the native code.)

  Shared memory should be available as long as the associated struct is not
  garbage collected.
  """
  @type t :: %__MODULE__{
          name: binary(),
          guard: reference(),
          size: non_neg_integer(),
          capacity: pos_integer()
        }

  @default_capacity 4096

  defstruct name: nil, guard: nil, size: 0, capacity: @default_capacity

  @doc """
  Creates a new, empty shared memory area with the given capacity
  """
  @spec empty(capacity :: pos_integer) :: t()
  def empty(capacity \\ @default_capacity) do
    {:ok, data} = create(capacity)
    data
  end

  @doc """
  Creates a new shared memory area filled with the existing data.
  """
  @spec new(binary()) :: t()
  def new(data) when is_binary(data) do
    new(data, byte_size(data))
  end

  @doc """
  Creates a new shared memory area initialized with `data` and sets its capacity.

  The actual capacity is the greater of passed capacity and data size
  """
  @spec new(data :: binary(), capacity :: pos_integer()) :: t()
  def new(data, capacity) when capacity > 0 do
    {:ok, shm} = create(capacity)
    {:ok, shm} = Native.write(shm, data)
    shm
  end

  @doc """
  Sets the capacity of shared memory area.

  If the capacity is smaller than the current size, data will be discarded and size modified
  """
  @spec set_capacity(t(), pos_integer()) :: t()
  def set_capacity(shm, capacity) do
    {:ok, new_shm} = Native.set_capacity(shm, capacity)
    new_shm
  end

  @doc """
  Ensures that shared memory is not garbage collected at the point of executing
  this function.

  Useful when passing shared memory to other OS process, to prevent it
  from being garbage collected until received and mapped by that process.
  """
  @spec ensure_not_gc(t()) :: :ok
  def ensure_not_gc(shm) do
    :ok = Native.ensure_not_gc(shm)
  end

  @doc """
  Returns shared memory contents as a binary.
  """
  @spec to_binary(t()) :: binary()
  def to_binary(shm) do
    {:ok, binary} = shm |> Native.read()
    binary
  end

  defp create(capacity) do
    shm_struct = %__MODULE__{capacity: capacity}
    Native.allocate(shm_struct)
  end
end
