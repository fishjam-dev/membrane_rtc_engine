defmodule Bundlex.Output do
  @moduledoc false

  @spec info(String.t()) :: :ok
  def info(msg) do
    Mix.shell().info("Bundlex: " <> msg)
  end

  @spec raise(binary()) :: no_return()
  def raise(msg) do
    Mix.raise("Bundlex: " <> msg)
  end
end
