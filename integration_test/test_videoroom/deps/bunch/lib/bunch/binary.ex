defmodule Bunch.Binary do
  @moduledoc """
  A bunch of helpers for manipulating binaries.
  """

  use Bunch

  @doc """
  Chunks given binary into parts of given size.

  Remaining part is cut off.

  ## Examples

      iex> <<1, 2, 3, 4, 5, 6>> |> #{inspect(__MODULE__)}.chunk_every(2)
      [<<1, 2>>, <<3, 4>>, <<5, 6>>]
      iex> <<1, 2, 3, 4, 5, 6, 7>> |> #{inspect(__MODULE__)}.chunk_every(2)
      [<<1, 2>>, <<3, 4>>, <<5, 6>>]

  """
  @spec chunk_every(binary, pos_integer) :: [binary]
  def chunk_every(binary, chunk_size) do
    {result, _} = chunk_every_rem(binary, chunk_size)
    result
  end

  @doc """
  Chunks given binary into parts of given size.

  Returns list of chunks and remainder.

  ## Examples

      iex> <<1, 2, 3, 4, 5, 6>> |> #{inspect(__MODULE__)}.chunk_every_rem(2)
      {[<<1, 2>>, <<3, 4>>, <<5, 6>>], <<>>}
      iex> <<1, 2, 3, 4, 5, 6, 7>> |> #{inspect(__MODULE__)}.chunk_every_rem(2)
      {[<<1, 2>>, <<3, 4>>, <<5, 6>>], <<7>>}

  """
  @spec chunk_every_rem(binary, chunk_size :: pos_integer) :: {[binary], remainder :: binary}
  def chunk_every_rem(binary, chunk_size) do
    do_chunk_every_rem(binary, chunk_size)
  end

  defp do_chunk_every_rem(binary, chunk_size, acc \\ []) do
    case binary do
      <<chunk::binary-size(chunk_size)>> <> rest ->
        do_chunk_every_rem(rest, chunk_size, [chunk | acc])

      rest ->
        {acc |> Enum.reverse(), rest}
    end
  end

  @doc """
  Cuts off the smallest possible chunk from the end of `binary`, so that the
  size of returned binary is an integer multiple of `i`.

  ## Examples

      iex> import #{inspect(__MODULE__)}
      iex> take_int_part(<<1,2,3,4,5,6,7,8>>, 3)
      <<1,2,3,4,5,6>>
      iex> take_int_part(<<1,2,3,4,5,6,7,8>>, 4)
      <<1,2,3,4,5,6,7,8>>

  """
  @spec take_int_part(binary, pos_integer) :: binary
  def take_int_part(binary, i) do
    {b, _} = split_int_part(binary, i)
    b
  end

  @doc """
  Returns a 2-tuple, where the first element is the result of `take_int_part(binary, i)`,
  and the second is the rest of `binary`.

  ## Examples

      iex> import #{inspect(__MODULE__)}
      iex> split_int_part(<<1,2,3,4,5,6,7,8>>, 3)
      {<<1,2,3,4,5,6>>, <<7,8>>}
      iex> split_int_part(<<1,2,3,4,5,6,7,8>>, 4)
      {<<1,2,3,4,5,6,7,8>>, <<>>}

  """
  @spec split_int_part(binary, pos_integer) :: {binary, binary}
  def split_int_part(binary, i) do
    len = Bunch.Math.max_multiple_lte(i, binary |> byte_size())
    <<b::binary-size(len), r::binary>> = binary
    {b, r}
  end
end
