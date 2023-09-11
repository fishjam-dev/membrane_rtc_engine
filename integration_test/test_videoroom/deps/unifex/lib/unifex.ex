defmodule Unifex do
  @moduledoc """
  Implementation of `Bundlex.Preprocessor`.

  When added to a native configuration in `Bundlex.Project`, equips the native with
  the Unifex native dependency and generated sources.
  """

  @behaviour Bundlex.Project.Preprocessor

  alias Bundlex.Native
  alias Bundlex.Project.Preprocessor
  alias Unifex.CodeGenerator
  alias Unifex.InterfaceIO

  @impl Preprocessor
  def preprocess_native_config(_name, _app, config) do
    unifex_deps = [unifex: :unifex]
    Keyword.update(config, :deps, unifex_deps, &(unifex_deps ++ &1))
  end

  @impl Preprocessor
  def preprocess_native(native) do
    %Native{app: app, name: name, interface: interface, language: language} = native

    {:ok, project_dir} = Bundlex.Helper.MixHelper.get_project_dir(app)

    generator =
      interface
      |> CodeGenerator.bundlex_interface()
      |> CodeGenerator.interface_generator()

    source =
      project_dir
      |> InterfaceIO.get_interfaces_specs!()
      |> Enum.find(fn {spec_name, _dir, _specs} -> spec_name == name end)
      |> case do
        {_spec_name, dir, _specs} -> InterfaceIO.out_path(name, dir, generator, ".#{language}")
        nil -> raise "Native #{inspect(name)} not found in app #{inspect(app)} specification"
      end

    %Native{native | sources: [source | native.sources]}
  end
end
