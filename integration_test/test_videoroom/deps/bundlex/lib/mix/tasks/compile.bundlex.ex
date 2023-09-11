defmodule Mix.Tasks.Compile.Bundlex do
  @shortdoc "Builds natives specified in bundlex.exs file"
  @moduledoc """
  #{@shortdoc}

  Accepts the following command line arguments:
  - `--store-scripts` - if set, shell scripts are stored in the project
  root folder for further analysis.

  Add `:bundlex` to compilers in your Mix project to have this task executed
  each time the project is compiled.
  """
  use Mix.Task.Compiler

  alias Bundlex.{BuildScript, Native, Output, Platform, Project}
  alias Bundlex.Helper.MixHelper

  @recursive true

  @impl true
  def run(_args) do
    {:ok, _apps} = Application.ensure_all_started(:bundlex)
    commands = []

    app = MixHelper.get_app!()
    platform = Bundlex.platform()

    project =
      with {:ok, project} <- Project.get(app) do
        project
      else
        {:error, reason} ->
          Output.raise("Cannot get project for app: #{inspect(app)}, reason: #{inspect(reason)}")
      end

    commands = commands ++ Platform.get_module(platform).toolchain_module.before_all!(platform)

    commands =
      commands ++
        case Native.resolve_natives(project, platform) do
          {:ok, nifs_commands} ->
            nifs_commands

          {:error, {app, reason}} ->
            Output.raise(
              "Error resolving natives for app #{inspect(app)}, reason: #{inspect(reason)}"
            )
        end

    build_script = BuildScript.new(commands)

    {cmdline_options, _argv, _errors} =
      OptionParser.parse(System.argv(), switches: [store_scripts: :boolean])

    if cmdline_options[:store_scripts] do
      {:ok, {filename, _script}} = build_script |> BuildScript.store(platform)
      Output.info("Stored build script at #{File.cwd!() |> Path.join(filename)}")
    end

    case build_script |> BuildScript.run(platform) do
      :ok ->
        :ok

      {:error, {:run_build_script, return_code: ret, command: cmd}} ->
        Output.raise("""
        Failed to build the native part of package #{app}. Errors may have been logged above.
        Make sure that all required packages are properly installed in your system.
        Requirements and installation guide may be found in the readme of package #{app}.

        Returned code: #{ret}
        Build script:

        #{cmd}
        """)

      {:error, reason} ->
        Output.raise("Error running build script, reason #{inspect(reason)}")
    end

    {:ok, []}
  end
end
