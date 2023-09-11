defmodule Bundlex.Platform.Windows64 do
  @moduledoc false
  use Bundlex.Platform

  @impl true
  def toolchain_module() do
    Bundlex.Toolchain.VisualStudio
  end
end
