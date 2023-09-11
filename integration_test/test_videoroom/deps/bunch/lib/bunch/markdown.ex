defmodule Bunch.Markdown do
  @moduledoc """
  A bunch of helpers for generating Markdown text
  """

  @doc """
  Indents whole block of text by specified number of spaces

  ## Examples

      iex>#{inspect(__MODULE__)}.indent("text")
      "  text"

      iex>text = \"""
      ...>First line
      ...>Second line
      ...>Third line
      ...>\"""
      iex>#{inspect(__MODULE__)}.indent(text)
      \"""
        First line
        Second line
        Third line
      \"""
      iex>#{inspect(__MODULE__)}.indent(text, 4)
      \"""
          First line
          Second line
          Third line
      \"""
  """

  @spec indent(String.t(), non_neg_integer()) :: String.t()
  def indent(string, level \\ 2) do
    do_indent(string, level)
  end

  @doc """
  Indents the whole block of text by specified number of hard spaces (`&nbsp;`).

  ## Examples

      iex>#{inspect(__MODULE__)}.hard_indent("text")
      "&nbsp;&nbsp;text"

      iex>text = \"""
      ...>First line
      ...>Second line
      ...>Third line
      ...>\"""
      iex>#{inspect(__MODULE__)}.hard_indent(text)
      \"""
      &nbsp;&nbsp;First line
      &nbsp;&nbsp;Second line
      &nbsp;&nbsp;Third line
      \"""
      iex>#{inspect(__MODULE__)}.hard_indent(text, 1)
      \"""
      &nbsp;First line
      &nbsp;Second line
      &nbsp;Third line
      \"""
  """
  @spec hard_indent(String.t(), non_neg_integer()) :: String.t()
  def hard_indent(string, level \\ 2) do
    do_indent(string, level, "&nbsp;")
  end

  defp do_indent(string, size, character \\ " ") do
    indent = String.duplicate(character, size)

    string
    |> String.replace("\n", "\n" <> indent)
    |> String.replace_suffix(indent, "")
    |> String.replace_prefix("", indent)
  end
end
