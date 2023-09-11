defmodule Membrane.RTP.Vad.AudioLevelQueue do
  @moduledoc false

  # The queue contains audio levels for VAD implementation. It is used as an input of IsSpeakingEstimator.estimate_is_speaking.
  # This structure builds on top of a simple FIFO Erlang queue by having a fixed max number of elements.

  # The newest element in always appended to the front and popped out from its rear, so `to_list/1` returns the most recent element as the head of a list.
  # The length of a list can be obtained in O(1) time.

  alias Membrane.RTP.Vad.VadParams

  @target_audio_level_length VadParams.target_levels_length()

  @enforce_keys [:levels, :length]
  defstruct [:levels, :length]

  @typedoc """
  A type for storing information about a fixed number of recent audio levels.

  `:levels` - erlang queue which stores at most @target_audio_level_length elements
  `:length` - number of elements
  """

  @type t() :: %__MODULE__{
          levels: :queue.queue(non_neg_integer()),
          length: non_neg_integer()
        }

  @doc """
  Creates new AudioLevelQueue.
  """
  @spec new(Enum.t()) :: t()
  def new(init_data \\ []) do
    levels =
      init_data
      |> Enum.take(@target_audio_level_length)
      |> Enum.to_list()
      |> :queue.from_list()

    %__MODULE__{levels: levels, length: :queue.len(levels)}
  end

  @doc """
  Given a AudioLevelQueue and level value it returns a queue with the level value on front

  The function also reduces the size of the queue if the maximum size has been reached.
  It does so by dropping the oldest level.
  """
  @spec add(t(), non_neg_integer) :: t()
  def add(%__MODULE__{length: @target_audio_level_length} = old_queue, level) do
    levels = :queue.in_r(level, :queue.drop_r(old_queue.levels))
    %__MODULE__{old_queue | levels: levels}
  end

  def add(%__MODULE__{levels: old_levels, length: length}, level) do
    %__MODULE__{levels: :queue.in_r(level, old_levels), length: length + 1}
  end

  @doc """
  Given an AudioLevelQueue it returns a list.
  """
  @spec to_list(t()) :: [non_neg_integer()]
  def to_list(%__MODULE__{levels: levels}), do: :queue.to_list(levels)
end
