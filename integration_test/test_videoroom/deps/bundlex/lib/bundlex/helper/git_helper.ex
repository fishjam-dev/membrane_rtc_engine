defmodule Bundlex.Helper.GitHelper do
  @moduledoc false
  # Module simplifying interaction with Git.

  @doc """
  Determines whether the [Git Large File Storage](https://git-lfs.github.com/) is
  activated on the current machine.
  """
  @spec lfs_present? :: boolean
  def lfs_present? do
    Mix.shell().cmd("git config --get-regexp ^filter\.lfs\.", quiet: true) == 0
  end
end
