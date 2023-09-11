defmodule Membrane.RTP.Header.Extension do
  @moduledoc """
  Describes RTP Header Extension defined in [RFC8285](https://datatracker.ietf.org/doc/html/rfc8285)
  and provides common functions for interacting with extensions placed in buffers.

  ```
   0                   1                   2
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 ...
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
  |  ID   |  len  |     data (len+1 bytes)   ...
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
  ```
  """
  alias Membrane.Buffer
  alias Membrane.RTP.SessionBin

  @enforce_keys [:identifier, :data]
  defstruct @enforce_keys

  @type identifier_t :: 1..14 | SessionBin.rtp_extension_name_t()

  @type t :: %__MODULE__{
          identifier: identifier_t(),
          data: binary()
        }

  @spec find(Buffer.t(), identifier_t()) :: t() | nil
  def find(%Buffer{metadata: %{rtp: %{extensions: extensions}}}, identifier) do
    Enum.find(extensions, &(&1.identifier == identifier))
  end

  @spec put(Buffer.t(), t()) :: Buffer.t()
  def put(buffer, extension) do
    Bunch.Struct.update_in(buffer, [:metadata, :rtp, :extensions], &[extension | &1])
  end

  @spec delete(Buffer.t(), identifier_t()) :: Buffer.t()
  def delete(buffer, identifier) do
    Bunch.Struct.update_in(
      buffer,
      [:metadata, :rtp, :extensions],
      &Enum.reject(&1, fn extension -> extension.identifier == identifier end)
    )
  end

  @spec pop(Buffer.t(), identifier_t()) :: {t() | nil, Buffer.t()}
  def pop(buffer, identifier) do
    case find(buffer, identifier) do
      nil -> {nil, buffer}
      extension -> {extension, delete(buffer, identifier)}
    end
  end
end
