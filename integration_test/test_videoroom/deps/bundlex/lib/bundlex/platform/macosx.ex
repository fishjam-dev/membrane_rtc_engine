defmodule Bundlex.Platform.MacOSX do
  @moduledoc false
  use Bundlex.Platform

  @impl true
  def toolchain_module() do
    Bundlex.Toolchain.XCode
  end
end
