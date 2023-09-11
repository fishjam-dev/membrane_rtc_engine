defmodule Bundlex.Helper.ErlangHelper do
  @moduledoc false
  # Module containing helper functions that ease determining path to locally-
  # installed Erlang.

  @doc """
  Tries to determine paths to includes directory of locally installed Erlang.
  """
  @spec get_includes(Bundlex.platform_t()) :: [String.t()]
  def get_includes(_platform) do
    [Path.join([:code.root_dir(), "usr", "include"])]
  end

  @doc """
  Tries to determine paths to libs directory of locally installed Erlang.
  """
  @spec get_lib_dirs(Bundlex.platform_t()) :: [String.t()]
  def get_lib_dirs(_platform) do
    [Path.join([:code.root_dir(), "usr", "lib"])]
  end
end
