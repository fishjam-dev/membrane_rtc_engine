defmodule Membrane.RTP.SequenceNumberTracker do
  @moduledoc """
  A module used to map 16-bit sequence number to a continuous index without rollovers.
  It also detectes repeated and lost packets
  """

  require Bitwise

  alias Membrane.RTP.Utils

  @seq_num_bits 16
  @rollover_modulus Bitwise.bsl(1, @seq_num_bits)

  @type t() :: %__MODULE__{
          highest_seen_index: nil | non_neg_integer()
        }
  defstruct highest_seen_index: nil

  @doc """
  Initializes new SequenceNumberTracker
  """
  @spec new() :: t()
  def new(), do: %__MODULE__{}

  @doc """
  Main function of the Tracker that returns a difference to the newest package and packet's monotonic index
  """
  @spec track(t(), Membrane.RTP.Header.sequence_number_t()) :: {integer(), non_neg_integer(), t()}
  def track(%__MODULE__{highest_seen_index: nil} = tracker, 0) do
    # Start from roc = 1 to allow late sequence numbers
    {1, @rollover_modulus, %__MODULE__{tracker | highest_seen_index: @rollover_modulus}}
  end

  def track(%__MODULE__{highest_seen_index: nil} = tracker, seq_num) do
    {1, seq_num, %__MODULE__{tracker | highest_seen_index: seq_num}}
  end

  def track(%__MODULE__{highest_seen_index: reference_index} = tracker, seq_num) do
    reference_seq_num = rem(reference_index, @rollover_modulus)
    reference_roc = div(reference_index, @rollover_modulus)

    incoming_roc =
      case Utils.from_which_rollover(reference_seq_num, seq_num, @rollover_modulus) do
        :current -> reference_roc
        :previous -> reference_roc - 1
        :next -> reference_roc + 1
      end

    incoming_index = seq_num + Bitwise.bsl(incoming_roc, @seq_num_bits)

    diff = incoming_index - reference_index

    tracker =
      if diff > 0 do
        %__MODULE__{tracker | highest_seen_index: incoming_index}
      else
        tracker
      end

    {diff, incoming_index, tracker}
  end
end
