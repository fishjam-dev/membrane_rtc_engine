defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger.Cache do
  @moduledoc false

  # Cache for RTPMunger
  # Stores information about mapping of sequence numbers from one space to another
  # It is a moving window with a size defined by difference of first and last sequence number present in the cache

  # !!! WARNING !!!
  # This module doesn't handle out-of-order entries, there isn't even a check as it's not meant to be used outside RTPMunger.
  # RTPMunger is responsible for ensuring that entries are added in appropriate order

  use Bunch.Access

  @max_seq_num 2 ** 16

  # History size is determined by the window size of SRTP Encryptor, which happens to have to be
  # at least 64
  @history_size 64

  defstruct cache: Qex.new()

  @type t() :: %__MODULE__{
          cache:
            Qex.t({original_seq_num :: non_neg_integer(), mapped_seq_num :: non_neg_integer()})
        }

  @spec new() :: t()
  def new(), do: %__MODULE__{}

  @spec push(t(), non_neg_integer(), non_neg_integer()) :: t()
  def push(%__MODULE__{} = state, from, to) do
    new_state = Map.update!(state, :cache, &Qex.push(&1, {from, to}))

    if Enum.empty?(state.cache) do
      new_state
    else
      {_last, last} = Qex.last!(state.cache)
      remove_outdated_entries(new_state, last)
    end
  end

  @spec get(t(), non_neg_integer()) :: {:ok, non_neg_integer()} | {:error, :not_found}
  def get(%__MODULE__{cache: cache}, from) do
    cache
    |> Enum.find(fn
      {^from, _to} -> true
      _otherwise -> false
    end)
    |> case do
      nil ->
        {:error, :not_found}

      {^from, to} ->
        {:ok, to}
    end
  end

  @spec remove_outdated_entries(t(), non_neg_integer()) :: t()
  def remove_outdated_entries(state, last_mapping) do
    window_size = get_window_size(state, last_mapping)

    if window_size >= @history_size do
      {_entry, cache} = Qex.pop(state.cache)
      remove_outdated_entries(%{state | cache: cache}, last_mapping)
    else
      state
    end
  end

  defp get_window_size(%{cache: cache} = _state, last_mapping) do
    case Qex.first(cache) do
      {:value, {_first, first_mapping}} ->
        rem(last_mapping - first_mapping + @max_seq_num, @max_seq_num)

      :empty ->
        0
    end
  end
end
