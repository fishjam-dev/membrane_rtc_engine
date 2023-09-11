defmodule Bundlex.Native do
  @moduledoc """
  Module responsible for parsing and processing natives' configurations.
  """

  use Bunch

  alias Bundlex.Helper.ErlangHelper
  alias Bundlex.{Output, Platform, Project}
  alias Bundlex.Project.Preprocessor

  @type name_t :: atom()
  @type interface_t :: :nif | :cnode | :port
  @type language_t :: :c | :cpp

  @type t :: %__MODULE__{
          name: atom,
          app: Application.app(),
          type: :native | :lib,
          includes: [String.t()],
          libs: [String.t()],
          lib_dirs: [String.t()],
          pkg_configs: [String.t()],
          sources: [String.t()],
          deps: [t],
          compiler_flags: [String.t()],
          linker_flags: [String.t()],
          language: language_t,
          interface: interface_t | nil,
          preprocessors: [Preprocessor.t()]
        }

  @enforce_keys [:name, :type]

  defstruct name: nil,
            app: nil,
            type: nil,
            includes: [],
            libs: [],
            lib_dirs: [],
            pkg_configs: [],
            sources: [],
            deps: [],
            compiler_flags: [],
            linker_flags: [],
            language: :c,
            interface: nil,
            preprocessors: []

  @project_keys Project.native_config_keys()

  @native_type_keys %{native: :natives, lib: :libs}

  @doc """
  Parses natives and generates compiler commands.
  """
  @spec resolve_natives(Project.t(), Bundlex.platform_t()) ::
          {:ok, compiler_commands :: [String.t()]}
          | {:error,
             {application :: atom,
              {:unknown_fields, [field :: atom]}
              | {:no_sources_in_native, native_name :: atom}
              | :invalid_project_specification
              | {:no_bundlex_project_in_file, path :: binary()}
              | :unknown_application}}
  def resolve_natives(project, platform) do
    case get_native_configs(project) do
      [] ->
        Output.info("No natives found")
        {:ok, []}

      native_configs ->
        erlang = %{
          includes: ErlangHelper.get_includes(platform),
          lib_dirs: ErlangHelper.get_lib_dirs(platform)
        }

        Output.info(
          "Building natives: #{native_configs |> Enum.map(& &1.name) |> Enum.uniq() |> Enum.join(", ")}"
        )

        native_configs
        |> Bunch.Enum.try_flat_map(&resolve_native(&1, erlang, project.src_path, platform))
    end
  end

  defp resolve_native(config, erlang, src_path, platform) do
    with {:ok, native} <- parse_native(config, src_path) do
      %__MODULE__{} =
        native = Enum.reduce(native.preprocessors, native, & &1.preprocess_native(&2))

      native =
        case native do
          %__MODULE__{type: :native, interface: :cnode} = native ->
            native
            |> Map.update!(:libs, &["pthread", "ei" | &1])
            |> Map.update!(:lib_dirs, &(erlang.lib_dirs ++ &1))

          %__MODULE__{} = native ->
            native
        end
        |> Map.update!(:includes, &(erlang.includes ++ &1))
        |> merge_deps()
        |> Map.update!(:sources, &Enum.uniq/1)
        |> Map.update!(:deps, fn deps -> Enum.uniq_by(deps, &{&1.app, &1.name}) end)

      commands = Platform.get_module(platform).toolchain_module.compiler_commands(native)
      {:ok, commands}
    end
  end

  defp parse_native(config, src_path) do
    {config, meta} = config |> Map.pop(:config)
    {preprocessors, config} = config |> Keyword.pop(:preprocessor, [])
    preprocessors = preprocessors |> Bunch.listify()

    config =
      Enum.reduce(preprocessors, config, & &1.preprocess_native_config(meta.name, meta.app, &2))

    {deps, config} = config |> Keyword.pop(:deps, [])
    interface = config |> Keyword.get(:interface)

    {src_base, config} = config |> Keyword.pop(:src_base, "#{meta.app}")

    withl fields: [] <- config |> Keyword.keys() |> Enum.reject(&(&1 in @project_keys)),
          do: native = (config ++ Enum.to_list(meta)) |> __struct__(),
          no_src: false <- native.sources |> Enum.empty?(),
          deps: {:ok, parsed_deps} <- parse_deps(deps, interface) do
      native =
        %__MODULE__{native | deps: parsed_deps, preprocessors: preprocessors}
        |> Map.update!(:includes, &[Path.join([src_path, src_base, ".."]) | &1])
        |> Map.update!(:sources, fn src ->
          src |> Enum.map(&Path.join([src_path, src_base, &1]))
        end)

      {:ok, native}
    else
      fields: fields -> {:error, {meta.app, {:unknown_fields, fields}}}
      no_src: true -> {:error, {meta.app, {:no_sources_in_native, native.name}}}
      deps: error -> error
    end
  end

  defp get_native_configs(project, types \\ [:lib, :native]) do
    types
    |> Bunch.listify()
    |> Enum.flat_map(fn type ->
      project.config
      |> Keyword.get(@native_type_keys[type], [])
      |> Enum.map(fn {name, config} ->
        %{config: config, name: name, type: type, app: project.app}
      end)
    end)
  end

  defp parse_deps(deps, interface) do
    deps
    |> Bunch.Enum.try_flat_map(fn {app, natives} ->
      parse_app_libs(app, natives |> Bunch.listify() |> MapSet.new(), interface)
    end)
  end

  defp parse_app_libs(app, names, interface) do
    withl project: {:ok, project} <- app |> Project.get(),
          do: libs = find_libs(project, names),
          libs: {:ok, libs} <- parse_libs(libs, project.src_path) do
      filter_libs(libs, names, interface)
    else
      project: {:error, reason} -> {:error, {app, reason}}
      libs: error -> error
    end
  end

  defp find_libs(project, names) do
    project |> get_native_configs(:lib) |> Enum.filter(&(&1.name in names))
  end

  defp filter_libs(libs, names, interface) do
    libs = Enum.filter(libs, &(&1.interface in [nil, interface]))
    diff = MapSet.difference(names, MapSet.new(libs, & &1.name))

    if diff |> Enum.empty?() do
      {:ok, libs}
    else
      {:error, {:libs_not_found, diff |> Enum.to_list()}}
    end
  end

  defp parse_libs(libs, src_path) do
    Bunch.Enum.try_map(libs, &parse_native(&1, src_path))
  end

  defp merge_deps(native) do
    native.deps |> Enum.map(&merge_deps/1) |> Enum.reduce(native, &merge_dep/2)
  end

  defp merge_dep(%__MODULE__{type: :lib} = dependency, %__MODULE__{} = native) do
    Map.merge(
      native,
      Map.take(dependency, [:includes, :libs, :lib_dirs, :pkg_configs, :linker_flags, :deps]),
      fn _k, v1, v2 -> v2 ++ v1 end
    )
  end
end
