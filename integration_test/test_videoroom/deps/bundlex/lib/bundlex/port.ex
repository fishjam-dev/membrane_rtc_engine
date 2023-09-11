defmodule Bundlex.Port do
  @moduledoc """
  Utilities to ease interaction with Ports.
  """

  alias Bundlex.Helper.MixHelper
  alias Bundlex.Native

  @doc """
  Spawns Port `native_name` from application of calling module.
  Returned result is compatible with standard Port API.
  """
  defmacro open(native_name, args \\ []) do
    app = MixHelper.get_app!(__CALLER__.module)

    quote do
      unquote(__MODULE__).open(unquote(app), unquote(native_name), unquote(args))
    end
  end

  @spec open(Application.app(), Native.name_t(), [String.t()]) :: port()
  def open(app, native_name, args) do
    Port.open(
      {:spawn_executable, Bundlex.build_path(app, native_name, :port)},
      args: args
    )
  end
end
