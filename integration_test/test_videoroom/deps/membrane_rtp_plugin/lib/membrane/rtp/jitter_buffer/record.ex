defmodule Membrane.RTP.JitterBuffer.Record do
  @moduledoc false

  # Describes a structure that is stored in the BufferStore.

  alias Membrane.RTP.JitterBuffer
  @enforce_keys [:index, :timestamp, :buffer]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          index: JitterBuffer.packet_index(),
          timestamp: Membrane.Time.t(),
          buffer: Membrane.Buffer.t()
        }

  @spec new(Membrane.Buffer.t(), JitterBuffer.packet_index()) :: t()
  def new(buffer, index) do
    %__MODULE__{
      index: index,
      timestamp: Membrane.Time.monotonic_time(),
      buffer: buffer
    }
  end

  @doc """
  Compares two records.

  Returns true if the first record is older than the second one.
  """
  # Designed to use with Heap: https://gitlab.com/jimsy/heap/blob/master/lib/heap.ex#L71
  @spec rtp_comparator(t(), t()) :: boolean()
  def rtp_comparator(%__MODULE__{index: l_index}, %__MODULE__{index: r_index}),
    do: l_index < r_index
end
