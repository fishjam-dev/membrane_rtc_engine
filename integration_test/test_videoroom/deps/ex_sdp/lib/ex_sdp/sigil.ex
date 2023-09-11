defmodule ExSDP.Sigil do
  @moduledoc """
  Module containing sigil for deleting empty lines and replacing `\n` with `\r\n`.
  """

  @spec sigil_n(String.t(), []) :: String.t()
  @doc """
  Deletes empty lines and replaces `\n` with `\r\n`
  """
  def sigil_n(string, []) do
    # replace multiple \n with one \n
    string = String.replace(string, ~r/(\n)+/, "\n")
    # replace \n with \r\n but only when \n is not preceded with \r
    String.replace(string, ~r/(?<!\r)\n/, "\r\n")
  end
end
