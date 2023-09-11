defmodule Bunch.ShortRef do
  @moduledoc """
  A wrapper over Erlang/Elixir references that makes them more readable and visually
  distinguishable.


  ## Erlang references
  When printed, Erlang references are quite long: `#Reference<0.133031758.722993155.68472>`.
  Moreover, since they're based on an incremented counter, it's hard to distinguish
  between a two created within the same period of time, like `#Reference<0.133031758.722993155.68512>`
  and `#Reference<0.133031758.722993155.68519>`.

  ## #{inspect(__MODULE__)}
  `t:#{inspect(__MODULE__)}.t/0` stores a usual reference along with first 4 bytes
  of its SHA1 hash and when inspected prints only 8 hex digits prepended with `#`.
  Thanks to use of the hash function, similar references have totally different
  string representations - the case from the previous example would be `#60e0fd2d`
  and `#d4208051`.

  ## When to use
  `#{inspect(__MODULE__)}` should be used when a reference is to be printed or logged.
  It should NOT be used when creating lots of references as it adds a significant
  performance overhead.
  """
  @enforce_keys [:ref, :hash]
  defstruct @enforce_keys

  @type t :: %__MODULE__{ref: reference, hash: String.t()}

  @doc """
  Creates a short reference.

      iex> IEx.Helpers.ref(0, 1, 2, 3) |> #{inspect(__MODULE__)}.new() |> inspect()
      "#82c033ef"
      iex> <<"#", hash::binary-size(8)>> = #{inspect(__MODULE__)}.new() |> inspect()
      iex> Base.decode16(hash, case: :lower) |> elem(0)
      :ok

  """
  @spec new(reference) :: t
  def new(ref \\ make_ref()) do
    ref_list = :erlang.ref_to_list(ref)
    <<bin_hash_part::binary-size(4), _dropped::binary>> = :crypto.hash(:sha, ref_list)
    hash = "#" <> Base.encode16(bin_hash_part, case: :lower)
    %__MODULE__{ref: ref, hash: hash}
  end
end

defimpl Inspect, for: Bunch.ShortRef do
  @impl true
  def inspect(%Bunch.ShortRef{hash: hash}, _opts), do: hash
end
