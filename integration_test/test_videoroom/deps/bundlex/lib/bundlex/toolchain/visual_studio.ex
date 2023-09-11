defmodule Bundlex.Toolchain.VisualStudio do
  @moduledoc false
  # Toolchain definition for Microsoft Visual Studio.
  #
  # It tries to determine Visual Studio root directory before compolation starts
  # and set up native.appropriate environment variables that will cause using right
  # compiler for given platform by calling vcvarsall.bat script shipped with
  # Visual Studio.
  #
  # Visual Studio directory may be override by setting VISUAL_STUDIO_ROOT
  # environment variable.

  use Bundlex.Toolchain

  alias Bundlex.Helper.{GitHelper, PathHelper}
  alias Bundlex.Native
  alias Bundlex.Output

  @directory_wildcard_x64 "c:\\Program Files (x86)\\Microsoft Visual Studio *"
  @directory_wildcard_x86 "c:\\Program Files\\Microsoft Visual Studio *"
  @directory_env "VISUAL_STUDIO_ROOT"

  @impl true
  def before_all!(:windows32) do
    [run_vcvarsall("x86")]
  end

  @impl true
  def before_all!(:windows64) do
    [run_vcvarsall("amd64")]
  end

  @impl true
  def compiler_commands(%Native{interface: :nif} = native) do
    # TODO escape quotes properly

    includes_part =
      Enum.map_join(native.includes, " ", fn include ->
        "/I \"#{PathHelper.fix_slashes(include)}\""
      end)

    sources_part =
      Enum.map_join(native.sources, " ", fn source -> "\"#{PathHelper.fix_slashes(source)}\"" end)

    if not (native.libs |> Enum.empty?()) and not GitHelper.lfs_present?() do
      Output.raise(
        "Git LFS is not installed, being necessary for downloading windows *.lib files for dlls #{inspect(native.libs)}. Install from https://git-lfs.github.com/."
      )
    end

    libs_part = Enum.join(native.libs, " ")

    unquoted_dir_part =
      native.app
      |> Toolchain.output_path(:nif)
      |> PathHelper.fix_slashes()

    dir_part = "\"#{unquoted_dir_part}\""

    [
      "if EXIST #{dir_part} rmdir /S /Q #{dir_part}",
      "mkdir #{dir_part}",
      "cl /LD #{includes_part} #{sources_part} #{libs_part} /link /DLL /OUT:\"#{Toolchain.output_path(native.app, native.name, :nif)}.dll\""
    ]
  end

  # Runs vcvarsall.bat script
  defp run_vcvarsall(vcvarsall_arg) do
    vcvarsall_path =
      determine_visual_studio_root()
      |> build_vcvarsall_path()

    case File.exists?(vcvarsall_path) do
      false ->
        Output.raise(
          "Unable to find vcvarsall.bat script within Visual Studio root directory. Is your Visual Studio installation valid? (and file is in VC directory?)"
        )

      true ->
        "if not defined VCINSTALLDIR call \"#{vcvarsall_path}\" #{vcvarsall_arg}"
    end
  end

  # Determines root directory of the Visual Studio.
  defp determine_visual_studio_root() do
    determine_visual_studio_root(System.get_env(@directory_env))
  end

  # Determines root directory of the Visual Studio.
  # Case when we don't have a root path passed via an environment variable.
  defp determine_visual_studio_root(nil) do
    visual_studio_path()
    |> determine_visual_studio_root_with_wildcard()
  end

  # Determines root directory of the Visual Studio.
  # Case when we have a root path passed via an environment variable.
  defp determine_visual_studio_root(directory) do
    directory
  end

  defp determine_visual_studio_root_with_wildcard(wildcard) do
    case PathHelper.latest_wildcard(wildcard) do
      nil ->
        Output.raise(
          "Unable to find Visual Studio root directory. Please ensure that it is either located in \"#{wildcard}\" or #{@directory_env} environment variable pointing to its root is set."
        )

      directory ->
        directory
    end
  end

  # Builds path to the vcvarsall.bat script that can be used to set environment
  # variables necessary to use Visual Studio compilers.
  defp build_vcvarsall_path(root) do
    Path.join([root, "VC", "vcvarsall.bat"])
  end

  defp visual_studio_path() do
    case :erlang.system_info(:wordsize) do
      4 -> @directory_wildcard_x86
      _word_size -> @directory_wildcard_x64
    end
  end
end
