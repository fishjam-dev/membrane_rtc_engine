defmodule Membrane.File.SeekEvent do
  @moduledoc """
  Event that triggers seeking or insertion to a file in `Membrane.File.Sink`.

  `inspect(__MODULE__)` allows to control behaviour of `Membrane.File.Sink` when writing to a file
  by seeking to given `position` (relative to beginning of file, current position or end of file).
  All buffers that arrive after that event are being written starting from the `position`.

  If overwriting isn't the desired behaviour, one can set `insert?` flag to `true`. This will trigger
  splitting the file into two parts containing bytes before and after `position`. All buffers following
  such an event will be appended to the first part. Inserting has considerable performance impact,
  especially if done repeatedly, so it should be used only as a fallback for variable-length data.
  For all other cases, one can fill unknown bytes with eg. `0`s and simply overwrite them later.
  """
  @derive Membrane.EventProtocol

  @type offset_t :: integer()
  @type position_t :: offset_t() | {:bof | :cur | :eof, offset_t()} | :bof | :cur | :eof

  @type t :: %__MODULE__{
          position: position_t(),
          insert?: boolean()
        }

  defstruct [:position, insert?: false]
end
