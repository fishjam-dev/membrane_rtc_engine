defmodule SecureRandom do
  use Bitwise

  @moduledoc """
  Takes my favorite hits from Ruby's SecureRandom and brings em to elixir.
  Mostly a convienance wrapper around Erlangs Crypto library, converting
  Crypto.strong_rand_bytes/1 into a string.

  ## Examples

      iex> SecureRandom.base64
      "xhTcitKZI8YiLGzUNLD+HQ=="

      iex> SecureRandom.urlsafe_base64(4)
      "pLSVJw"

      iex> SecureRandom.uuid
      "a18e8302-c417-076d-196a-71dfbd5b1e03"

  """

  @default_length 16

  @doc """
  Returns random Base64 encoded string.

  ## Examples

      iex> SecureRandom.base64
      "rm/JfqH8Y+Jd7m5SHTHJoA=="

      iex> SecureRandom.base64(8)
      "2yDtUyQ5Xws="

  """
  def base64(n \\ @default_length) do
    random_bytes(n)
    |> Base.encode64(case: :lower)
  end

  @doc """
  Generates a random hexadecimal string.

  The argument n specifies the length, in bytes, of the random number to be generated. The length of the resulting hexadecimal string is twice n.

  If n is not specified, 16 is assumed. It may be larger in future.

  The result may contain 0-9 and a-f.

  ## Examples

      iex> SecureRandom.hex(6)
      "34fb5655a231"
  """
  def hex(n \\ @default_length) do
    random_bytes(n)
    |> Base.encode16(case: :lower)
  end

  @doc """
  Returns random urlsafe Base64 encoded string.

  ## Examples

      iex> SecureRandom.urlsafe_base64
      "xYQcVfWuq6THMY_ZVmG0mA"

      iex> SecureRandom.urlsafe_base64(8)
      "8cN__l-6wNw"

  """
  def urlsafe_base64(n \\ @default_length) do
    base64(n)
    |> Base.url_encode64(case: :lower, padding: true)
  end

  @doc """
  Returns UUID v4 string. I have lifted most of this straight from Ecto's implementation.

  ## Examples

    iex> SecureRandom.uuid
    "e1d87f6e-fbd5-6801-9528-a1d568c1fd02"
  """
  def uuid do
    bigenerate() |> encode
  end

  @doc """
  Returns random bytes.

  ## Examples

      iex> SecureRandom.random_bytes
      <<202, 104, 227, 197, 25, 7, 132, 73, 92, 186, 242, 13, 170, 115, 135, 7>>

      iex> SecureRandom.random_bytes(8)
      <<231, 123, 252, 174, 156, 112, 15, 29>>

  """
  def random_bytes(n \\ @default_length) do
    :crypto.strong_rand_bytes(n)
  end

  defp bigenerate do
    <<u0::48, _::4, u1::12, _::2, u2::62>> = random_bytes(16)
    <<u0::48, 4::4, u1::12, 2::2, u2::62>>
  end

  defp encode(<<u0::32, u1::16, u2::16, u3::16, u4::48>>) do
    hex_pad(u0, 8) <> "-" <>
    hex_pad(u1, 4) <> "-" <>
    hex_pad(u2, 4) <> "-" <>
    hex_pad(u3, 4) <> "-" <>
    hex_pad(u4, 12)
  end

  defp hex_pad(hex, count) do
    hex = Integer.to_string(hex, 16)
    lower(hex, :binary.copy("0", count - byte_size(hex)))
  end

  defp lower(<<h, t::binary>>, acc) when h in ?A..?F,
    do: lower(t, acc <> <<h + 32>>)
  defp lower(<<h, t::binary>>, acc),
    do: lower(t, acc <> <<h>>)
  defp lower(<<>>, acc),
    do: acc
end
