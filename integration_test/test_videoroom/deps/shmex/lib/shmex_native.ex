defmodule Shmex.Native do
  @moduledoc """
  This module provides natively implemented functions allowing low-level
  operations on Posix shared memory. Use with caution!
  """

  use Bundlex.Loader, nif: :shmex

  @doc """
  Creates shared memory segment and adds a guard for it.

  The guard associated with this memory segment is placed in returned
  `Shmex` struct. When the guard resource is deallocated by BEAM,
  the shared memory is unlinked and will disappear from the system when last process
  using it unmaps it
  """
  @spec allocate(Shmex.t()) ::
          {:ok, Shmex.t()} | {:error, {:file.posix(), :ftruncate}}
  defnif allocate(shm)

  @doc """
  Creates guard for existing shared memory.

  This function should be only used when `Shmex` struct was created by
  some other NIF and even though the SHM exists, it's guard field is set to `nil`.
  Trying to use it with SHM obtained via `allocate/1` will result in error.

  See also docs for `allocate/1`
  """
  @spec add_guard(Shmex.t()) :: {:ok, Shmex.t()} | {:error, :already_guarded}
  defnif add_guard(shm)

  @doc """
  Sets the capacity of shared memory area and updates the Shmex struct accordingly.
  """
  @spec set_capacity(Shmex.t(), capacity :: pos_integer()) ::
          {:ok, Shmex.t()} | {:error, {:file.posix(), :shm_open | :ftruncate}}
  defnif set_capacity(shm, capacity)

  @doc """
  Reads the contents of shared memory and returns it as a binary.
  """
  @spec read(Shmex.t()) :: {:ok, binary} | {:error, {:file.posix(), :shm_open | :mmap}}
  def read(%Shmex{size: size} = shm) do
    read(shm, size)
  end

  @doc """
  Reads `cnt` bytes from the shared memory and returns it as a binary.

  `cnt` should not be greater than `shm.size`
  """
  @spec read(Shmex.t(), read_size :: non_neg_integer()) ::
          {:ok, binary()} | {:error, :invalid_read_size | {:file.posix(), :shm_open | :mmap}}
  defnif read(shm, read_size)

  @doc """
  Writes the binary into the shared memory.

  Overwrites the existing content. Increases the capacity of shared memory
  to fit the data.
  """
  @spec write(Shmex.t(), data :: binary()) ::
          {:ok, Shmex.t()} | {:error, {:file.posix(), :shm_open | :mmap}}
  defnif write(shm, data)

  @doc """
  Splits the contents of shared memory area into two by moving the data past
  the specified position into a new shared memory.

  `shm` has to be an existing shared memory (obtained via `allocate/1`).

  It virtually trims the existing shared memory to `position` bytes
  by setting `size` to `position` (The actual data is still present)
  and the overlapping data is copied into the new shared memory area.
  """
  @spec split_at(Shmex.t(), position :: non_neg_integer()) ::
          {:ok, {Shmex.t(), Shmex.t()}}
          | {:error, {:file.posix(), :shm_open | :mmap | :ftruncate}}
  defnif split_at(shm, position)

  @doc """
  Concatenates two shared memory areas by appending the data from the second
  at the end of the first one. Fails with `{:error, {:einval, :ftruncate}}` if
  OS does not support changing shared memory capacity.

  The first shared memory is a target that will contain data from both shared memory areas.
  Its capacity will be set to the sum of sizes of both shared memory areas.
  The second one, the source, will remain unmodified.
  """
  @spec append(target :: Shmex.t(), source :: Shmex.t()) ::
          {:ok, Shmex.t()} | {:error, {:file.posix(), :shm_open | :mmap | :ftruncate}}
  defnif append(target, source)

  @doc """
  Ensures that shared memory is not garbage collected at the point of executing
  this function.

  Useful when passing shared memory to other OS process, to prevent it
  from being garbage collected until received and mapped by that process.
  """
  @spec ensure_not_gc(Shmex.t()) :: :ok
  defnif ensure_not_gc(shm)

  @doc """
  Trims shared memory capacity to match its size.
  """
  @spec trim(Shmex.t()) :: {:ok, Shmex.t()} | {:error, {:file.posix(), :shm_open | :ftruncate}}
  def trim(%Shmex{size: size} = shm) do
    shm |> set_capacity(size)
  end

  @doc """
  Drops `bytes` bytes from the beginning of shared memory area and
  trims it to match the new size.
  """
  @spec trim(Shmex.t(), bytes :: non_neg_integer) ::
          {:ok, Shmex.t()} | {:error, {:file.posix(), :shm_open | :mmap}}
  def trim(shm, bytes) do
    with {:ok, trimmed_front} <- trim_leading(shm, bytes),
         {:ok, result} <- trim(trimmed_front) do
      {:ok, result}
    end
  end

  defnifp trim_leading(shm, offset)
end
