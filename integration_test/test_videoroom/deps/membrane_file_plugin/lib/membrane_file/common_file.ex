defmodule Membrane.File.CommonFile do
  @moduledoc false

  @behaviour Membrane.File.CommonFileBehaviour

  alias Membrane.{Buffer, Payload}

  @type posix_error_t :: {:error, File.posix()}
  @type generic_error_t :: {:error, File.posix() | :badarg | :terminated}

  @impl true
  def open(path, modes), do: File.open(path, [:binary | List.wrap(modes)])

  @impl true
  def open!(path, modes) do
    case open(path, modes) do
      {:ok, io_device} -> io_device
      {:error, posix_error} -> raise "Failed to open file '#{path}': #{inspect(posix_error)}"
    end
  end

  @impl true
  def write(fd, %Buffer{payload: payload}), do: IO.binwrite(fd, Payload.to_binary(payload))

  @impl true
  def write!(fd, buffer) do
    case write(fd, buffer) do
      :ok -> :ok
      {:error, error} -> raise "Failed to write to a file #{inspect(fd)}: #{inspect(error)}"
    end
  end

  @impl true
  def seek(fd, position), do: :file.position(fd, position)

  @impl true
  def seek!(fd, position) do
    case seek(fd, position) do
      {:ok, new_position} ->
        new_position

      {:error, error} ->
        raise "Failed to seek #{inspect(fd)} to position #{position}: #{inspect(error)}"
    end
  end

  @impl true
  def copy(source_fd, destination_fd) do
    with {:ok, src_position} <- :file.position(source_fd, :cur),
         {:ok, dst_position} <- :file.position(destination_fd, :cur),
         {:ok, bytes_copied} <- :file.copy(source_fd, destination_fd),
         {:ok, _src_position} <- :file.position(source_fd, src_position),
         {:ok, _dst_position} <- :file.position(destination_fd, dst_position) do
      {:ok, bytes_copied}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def copy!(src_fd, dest_fd) do
    case copy(src_fd, dest_fd) do
      {:ok, bytes_copied} ->
        bytes_copied

      {:error, reason} ->
        raise "Failed to copy #{inspect(src_fd)} to #{inspect(dest_fd)}: #{inspect(reason)}"
    end
  end

  @impl true
  def split(source_fd, destination_fd) do
    with {:ok, _bytes_copied} <- copy(source_fd, destination_fd),
         :ok <- truncate(source_fd) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def split!(src_fd, dest_fd) do
    case split(src_fd, dest_fd) do
      :ok ->
        :ok

      {:error, reason} ->
        raise "Failed to split #{inspect(src_fd)} into #{inspect(dest_fd)}: #{inspect(reason)}"
    end
  end

  @impl true
  defdelegate truncate(fd), to: :file

  @impl true
  def truncate!(fd) do
    case truncate(fd) do
      :ok -> :ok
      {:error, reason} -> raise "Failed to truncate file #{inspect(fd)}: #{inspect(reason)}"
    end
  end

  @impl true
  defdelegate close(fd), to: File

  @impl true
  def close!(fd) do
    case close(fd) do
      :ok ->
        :ok

      {:error, reason} ->
        raise "Failed to close file #{inspect(fd)}: #{inspect(reason)}"
    end
  end

  @impl true
  defdelegate rm(path), to: File

  @impl true
  defdelegate rm!(path), to: File

  @impl true
  defdelegate binread(fd, bytes_count), to: IO

  @impl true
  def binread!(fd, bytes_count) do
    case binread(fd, bytes_count) do
      {:error, reason} -> raise "Failed to read from #{inspect(fd)}: #{inspect(reason)}"
      result -> result
    end
  end
end
