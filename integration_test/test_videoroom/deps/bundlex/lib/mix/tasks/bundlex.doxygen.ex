defmodule Mix.Tasks.Bundlex.Doxygen do
  @shortdoc "Generates doxygen documentation for Bundlex project"
  @moduledoc """
  #{@shortdoc}

  Accepts the following command line arguments:
  - `--yes`, `-y` - skips confirmation prompt and overwrites existing meta files
  - `--no`, `-n` - skips confirmation prompt and does not overwrite existing meta files
  """

  use Mix.Task

  alias Bundlex.{Doxygen, Output, Project}
  alias Bundlex.Helper.MixHelper

  @impl Mix.Task
  def run(args) do
    {:ok, _apps} = Application.ensure_all_started(:bundlex)

    skip_overwrite_check? = "-y" in args or "--yes" in args
    always_overwrite? = "-n" in args or "--no" in args

    if skip_overwrite_check? and always_overwrite? do
      Mix.raise("Cannot use both --yes and --no options")
    end

    app = MixHelper.get_app!()

    project = get_project(app)

    doxygen = Doxygen.doxygen(project)

    Doxygen.generate_doxyfile(doxygen)

    Doxygen.generate_doxygen_documentation(doxygen)

    if skip_overwrite_check? do
      Doxygen.generate_hex_page(doxygen)
    else
      overwrite? =
        always_overwrite? or Mix.shell().yes?("Do you want to overwrite existing hex page?")

      if overwrite? do
        Doxygen.generate_hex_page(doxygen)
      else
        Output.info("Skipping hex page generation")
      end
    end

    unless page_included?(doxygen.page_path) do
      example_docs = """
      defp docs do
        [
          extras: [
            "#{doxygen.page_path}",
            ...
          ],
          ...
        ]
      end
      """

      Output.info("""
      Doxygen documentation page is not included in the project docs.
      Add the following snippet to your mix.exs file:
      #{example_docs}
      """)
    end
  end

  defp get_project(app) do
    with {:ok, project} <- Project.get(app) do
      project
    else
      {:error, reason} ->
        Output.raise("Cannot get project for app: #{inspect(app)}, reason: #{inspect(reason)}")
    end
  end

  defp page_included?(doxygen_page) do
    config = Mix.Project.config()

    with {:ok, docs} <- Keyword.fetch(config, :docs),
         {:ok, extras} <- Keyword.fetch(docs, :extras) do
      doxygen_page in extras
    else
      :error -> false
    end
  end
end
