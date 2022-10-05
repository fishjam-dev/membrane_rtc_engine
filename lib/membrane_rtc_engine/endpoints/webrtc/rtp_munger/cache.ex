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
  @history_size div(@max_seq_num, 8)

  defstruct cache: Qex.new()

  @type t() :: %__MODULE__{
          cache:
            Qex.t({original_seq_num :: non_neg_integer(), mapped_seq_num :: non_neg_integer()})
        }

  @spec new() :: t()
  def new(), do: %__MODULE__{}

  @spec push(t(), non_neg_integer(), non_neg_integer()) :: t()
  def push(%__MODULE__{} = state, from, to) do
    state
    |> Map.update!(:cache, &Qex.push(&1, {from, to}))
    |> remove_outdated_entries()
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

  # FIXME: this needs to be called every time RTPMunger sees a buffer
  # Calling it only when writing to a cache is not enough
  defp remove_outdated_entries(state) do
    window_size = get_window_size(state)

    if window_size >= @history_size do
      {_entry, cache} = Qex.pop(state.cache)
      remove_outdated_entries(%{state | cache: cache})
    else
      state
    end
  end

  defp get_window_size(%{cache: cache} = _state) do
    {first, _mapped_first} = cache |> Qex.first!()
    {last, _mapped_last} = cache |> Qex.last!()

    rem(last - first + @max_seq_num, @max_seq_num)
  end
end
