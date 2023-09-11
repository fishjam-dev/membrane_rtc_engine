defmodule Unifex.Utils do
  @moduledoc false

  @spec clang_format_installed?() :: boolean()
  def clang_format_installed?(), do: System.find_executable("clang-format") != nil
end
