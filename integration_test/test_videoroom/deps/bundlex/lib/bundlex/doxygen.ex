defmodule Bundlex.Doxygen do
  @moduledoc """
  Module responsible for generating doxygen documentation for Bundlex projects.
  """

  alias Bundlex.Project

  @type doxygen_t :: %{
          project_name: String.t(),
          doxyfile_path: String.t(),
          doxygen_path: String.t(),
          page_path: String.t()
        }

  @doc """
  Prepares struct with all necessary filepaths for the native documentation
  """
  @spec doxygen(Project.t()) :: doxygen_t()
  def doxygen(project) do
    project_name = Atom.to_string(project.app)

    %{
      project_name: project_name,
      doxyfile_path: doxyfile_path(project),
      doxygen_path: doxygen_path(project),
      page_path: page_path(project)
    }
  end

  defp doxyfile_path(project) do
    project_name = "#{project.app}"
    project_dirpath = Path.join(["doc", "doxygen", project_name])
    Path.join(project_dirpath, "Doxyfile")
  end

  defp doxygen_path(project) do
    project_name = "#{project.app}"
    Path.join(["doc", "doxygen", project_name])
  end

  defp page_path(project) do
    Path.join(["pages", "doxygen", "#{project.app}.md"])
  end

  @doc """
  Generates doxyfile in the c_src/project directory for Bundlex project.
  """
  @spec generate_doxyfile(doxygen_t()) :: :ok
  def generate_doxyfile(doxygen) do
    create_doxyfile_template(doxygen)

    keywords_to_change = keywords_to_change(doxygen)
    update_doxyfile_keywords(doxygen, keywords_to_change)
  end

  defp create_doxyfile_template(doxygen) do
    ensure_doxygen_dir_existence(doxygen)
    cmd_bundlex(["-g", doxygen.doxyfile_path])
  end

  defp cmd_bundlex(args) do
    with {_res, 0} <-
           System.cmd("doxygen", args) do
      :ok
    else
      {output, status} ->
        error_message =
          "Running doxygen command with args: #{inspect(args)} failed with exit code: #{inspect(status)} and output: #{inspect(output)}"

        raise error_message
    end
  rescue
    e in ErlangError ->
      case e do
        %ErlangError{original: :enoent} ->
          reraise Bundlex.Doxygen.Error,
                  [
                    message:
                      "Failed to run `doxygen` command with args #{inspect(args)}. Ensure, that you have `doxygen` available on your machine"
                  ],
                  __STACKTRACE__

        e ->
          reraise e, __STACKTRACE__
      end
  end

  defp keywords_to_change(doxygen) do
    %{
      "PROJECT_NAME" => doxygen.project_name,
      "OUTPUT_DIRECTORY" => doxygen.doxygen_path,
      "TAB_SIZE" => "2",
      "BUILTIN_STL_SUPPORT" => "YES",
      "RECURSIVE" => "YES",
      "GENERATE_LATEX" => "NO",
      "INPUT" => Path.join(["c_src", doxygen.project_name]),
      "EXTRACT_STATIC" => "YES"
    }
  end

  defp update_doxyfile_keywords(doxygen, keywords_to_change) do
    doxyfile_path = doxygen.doxyfile_path

    File.stream!(doxyfile_path)
    |> Stream.map(fn line ->
      if comment?(line) do
        line
      else
        replace_keywords(line, keywords_to_change)
      end
    end)
    |> Enum.join()
    |> then(&File.write!(doxyfile_path, &1))
  end

  defp comment?(line) do
    String.starts_with?(line, "#")
  end

  defp replace_keywords(line, keywords_to_change) do
    Enum.reduce(keywords_to_change, line, fn {keyword, value}, acc ->
      String.replace(acc, ~r/(#{keyword}\s*=)\s*(.*)\n/, "\\1 \"#{value}\"\n")
    end)
  end

  @doc """
  Generates html doxygen documentation for the Bundlex project. Doxyfile must be generated before.
  """
  @spec generate_doxygen_documentation(doxygen_t()) :: :ok
  def generate_doxygen_documentation(doxygen) do
    ensure_doxygen_dir_existence(doxygen)

    cmd_bundlex([doxygen.doxyfile_path])
  end

  defp ensure_doxygen_dir_existence(doxygen) do
    if not File.exists?(doxygen.doxygen_path) do
      File.mkdir_p!(doxygen.doxygen_path)
    end

    File.touch!(Path.join(["doc", ".build"]))
  end

  @doc """
  Generates page for the Bundlex project in the pages/doxygen directory.
  Page must be manually added to the docs extras in the mix.exs.
  Page contains only link to the doxygen html documentation.
  """
  @spec generate_hex_page(doxygen_t()) :: :ok
  def generate_hex_page(doxygen) do
    pages_dirpath = Path.dirname(doxygen.page_path)

    if not File.exists?(pages_dirpath) do
      File.mkdir_p!(pages_dirpath)
    end

    [_doc | doxygen_path] = Path.split(doxygen.doxygen_path)

    html_filepath = Path.join(["."] ++ doxygen_path ++ ["html", "index.html"])

    page = """
    # Native code documentation
    [Doxygen documentation of the native code](#{html_filepath})
    """

    File.write!(doxygen.page_path, page)
  end
end
