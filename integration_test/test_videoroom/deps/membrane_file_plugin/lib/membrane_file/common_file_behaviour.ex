defmodule Membrane.File.CommonFileBehaviour do
  @moduledoc false

  alias Membrane.Buffer
  alias Membrane.File.SeekEvent

  @common_file_impl Application.compile_env(
                      :membrane_file_plugin,
                      :file_impl,
                      Membrane.File.CommonFile
                    )

  @spec get_impl() :: module()
  def get_impl(), do: @common_file_impl

  @type posix_error_t :: {:error, File.posix()}
  @type generic_error_t :: {:error, File.posix() | :badarg | :terminated}

  @callback open(Path.t(), File.mode() | [File.mode() | :ram]) ::
              {:ok, File.io_device()} | posix_error_t()
  @callback open!(Path.t(), File.mode() | [File.mode() | :ram]) :: File.io_device()

  @callback write(File.io_device(), Buffer.t()) :: :ok | posix_error_t()
  @callback write!(File.io_device(), Buffer.t()) :: :ok

  @callback seek(File.io_device(), SeekEvent.position_t()) ::
              {:ok, integer()} | generic_error_t()
  @callback seek!(File.io_device(), SeekEvent.position_t()) :: integer()

  @callback copy(File.io_device(), File.io_device()) ::
              {:ok, non_neg_integer()} | generic_error_t()
  @callback copy!(File.io_device(), File.io_device()) :: non_neg_integer()

  @callback split(File.io_device(), File.io_device()) :: :ok | generic_error_t()
  @callback split!(File.io_device(), File.io_device()) :: :ok

  @callback truncate(File.io_device()) :: :ok | generic_error_t()
  @callback truncate!(File.io_device()) :: :ok

  @callback close(File.io_device()) :: :ok | posix_error_t()
  @callback close!(File.io_device()) :: :ok

  @callback rm(Path.t()) :: :ok | posix_error_t()
  @callback rm!(Path.t()) :: :ok

  @callback binread(File.io_device(), non_neg_integer()) :: iodata() | IO.nodata()
  @callback binread!(File.io_device(), non_neg_integer()) :: iodata() | :eof
end
