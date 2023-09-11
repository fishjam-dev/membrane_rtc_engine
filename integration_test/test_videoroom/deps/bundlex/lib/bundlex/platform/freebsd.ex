defmodule Bundlex.Platform.Freebsd do
  @moduledoc false
  use Bundlex.Platform

  @impl true
  def toolchain_module() do
    Bundlex.Toolchain.Clang
  end
end
