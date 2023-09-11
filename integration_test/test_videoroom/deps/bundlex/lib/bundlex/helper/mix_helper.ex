defmodule Bundlex.Helper.MixHelper do
  @moduledoc false
  # Module containing helper functions that ease retrieving certain values from
  # Mix configuration files.

  use Bunch
  alias Bundlex.Output

  @doc """
  Helper function for retrieving app name from mix.exs and failing if it was
  not found.
  """
  @spec get_app! :: atom
  def get_app!() do
    case Mix.Project.config()[:app] do
      nil ->
        Output.raise(
          "Unable to determine app name, check if :app key is present in return value of project/0 in mix.exs"
        )

      app ->
        app
    end
  end

  @doc """
  Returns app for the given module. In case of failure fallbacks to `get_app!/0`.
  """
  @spec get_app!(module) :: atom
  def get_app!(module) do
    Application.get_application(module) || get_app!()
  end

  @doc """
  Returns path to the `priv` dir for given application.
  """
  @spec get_priv_dir(application :: atom) :: String.t()
  def get_priv_dir(application \\ get_app!()) do
    # It seems that we have two methods to determine where Natives are located:
    # * `Mix.Project.build_path/0`
    # * `:code.priv_dir/1`
    #
    # Both seem to be unreliable, at least in Elixir 1.7:
    #
    # * we cannot call `Mix.Project.build_path/0` from `@on_load` handler as
    #   there are race conditions and it seems that some processes from the
    #   `:mix` app are not launched yet (yes, we tried to ensure that `:mix`
    #   app is started, calling `Application.ensure_all_started(:mix)` prior
    #   to calling `Mix.Project.build_path/0` causes deadlock; calling it
    #   without ensuring that app is started terminates the whole app; adding
    #   `:mix` to bundlex OTP applications does not seem to help either),
    # * it seems that when using releases, `Mix.Project.build_path/0` returns
    #   different results in compile time and run time,
    # * moreover, it seems that the paths returned by `Mix.Project.build_path/0`
    #   when using releases and `prod` env might have a `dev` suffix unless
    #   a developer remembers to disable `:build_per_environment: setting,
    # * `:code.priv_dir/1` is not accessible in the compile time, but at least
    #   it does not crash anything.
    #
    # As a result, we try to call `:code.priv_dir/1` first, and if it fails,
    # we are probably in the build time and need to fall back to
    # `Mix.Project.build_path/0`. Previously the check was reversed and it
    # caused crashes at least when using distillery >= 2.0 and Elixir 1.7.
    #
    # Think twice before you're going to be another person who spent many
    # hours on trying to figure out why such simple thing as determining
    # a path might be so hard.
    case :code.priv_dir(application) do
      {:error, :bad_name} ->
        [Mix.Project.build_path(), "lib", "#{application}", "priv"] |> Path.join()

      path ->
        path
    end
  end

  @doc """
  Returns root directory of the currently compiled project.
  """
  @spec get_project_dir() :: {:ok, binary}
  def get_project_dir() do
    {:ok, File.cwd!()}
  end

  @doc """
  Returns root directory of the project of given application.
  """
  @spec get_project_dir(application :: atom) :: {:ok, binary} | {:error, :unknown_application}
  def get_project_dir(application) do
    if application == get_app!() do
      get_project_dir()
    else
      case Mix.Project.deps_paths()[application] do
        nil -> {:error, :unknown_application}
        path -> {:ok, path}
      end
    end
  end
end
