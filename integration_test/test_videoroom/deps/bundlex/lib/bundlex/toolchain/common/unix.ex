defmodule Bundlex.Toolchain.Common.Unix do
  @moduledoc false

  use Bunch
  alias Bundlex.{Native, Output, Toolchain}
  alias Bundlex.Toolchain.Common.Compilers

  @spec compiler_commands(
          Native.t(),
          compile :: String.t(),
          link :: String.t(),
          lang :: Native.language_t(),
          options :: Keyword.t()
        ) :: [String.t()]
  def compiler_commands(native, compile, link, lang, options \\ []) do
    includes = native.includes |> paths("-I")
    pkg_config_cflags = pkg_config(native, :cflags)
    compiler_flags = resolve_compiler_flags(native.compiler_flags, native.interface, lang)
    output = Toolchain.output_path(native.app, native.name, native.interface)
    output_obj = output <> "_obj"

    objects =
      native.sources
      |> Enum.map(fn source ->
        """
        #{Path.join(output_obj, source |> Path.basename())}_\
        #{:crypto.hash(:sha, source) |> Base.encode16()}.o\
        """
      end)

    compile_commands =
      native.sources
      |> Enum.zip(objects)
      |> Enum.map(fn {source, object} ->
        """
        #{compile} -Wall -Wextra -c -O2 -g #{compiler_flags} \
        -o #{path(object)} #{includes} #{pkg_config_cflags} #{path(source)}
        """
      end)

    ["mkdir -p #{path(output_obj)}"] ++
      compile_commands ++ link_commands(native, link, output, objects, options)
  end

  defp resolve_compiler_flags(compiler_flags, interface, lang) do
    compiler_flags
    |> add_interface_macro_flag(interface)
    |> maybe_add_std_flag(lang)
    |> Enum.join(" ")
  end

  defp add_interface_macro_flag(compiler_flags, nil) do
    compiler_flags
  end

  defp add_interface_macro_flag(compiler_flags, interface) do
    macro_flag = "-DBUNDLEX_#{interface |> Atom.to_string() |> String.upcase()}"
    [macro_flag] ++ compiler_flags
  end

  defp maybe_add_std_flag(compiler_flags, lang) do
    if standard_specyfied?(compiler_flags) do
      compiler_flags
    else
      flag = Compilers.get_default_std_flag(lang)
      [flag | compiler_flags]
    end
  end

  defp standard_specyfied?(compiler_flags) do
    Enum.any?(compiler_flags, &String.match?(&1, ~r/^-std=/))
  end

  defp link_commands(%Native{type: :lib}, _link, output, objects, _options) do
    a_path = path(output <> ".a")
    ["rm -f #{a_path}", "ar rcs #{a_path} #{paths(objects)}"]
  end

  defp link_commands(native, link, output, objects, options) do
    extension =
      case native.interface do
        :nif -> ".so"
        interface when interface in [:cnode, :port] -> ""
      end

    wrap_deps = options |> Keyword.get(:wrap_deps, & &1)

    deps =
      native.deps
      |> Enum.map(&(Toolchain.output_path(&1.app, &1.name, &1.interface) <> ".a"))
      |> paths()
      |> wrap_deps.()

    linker_flags = native.linker_flags |> Enum.join(" ")

    [
      """
      #{link} #{linker_flags} -o #{path(output <> extension)} \
      #{deps} #{paths(objects)} #{libs(native)}
      """
    ]
  end

  defp paths(paths, flag \\ "") do
    Enum.map_join(paths, " ", fn p -> "#{flag}#{path(p)}" end)
  end

  defp path(path) do
    path = path |> String.replace(~S("), ~S(\")) |> Path.expand()
    ~s("#{path}")
  end

  defp libs(native) do
    lib_dirs = native.lib_dirs |> paths("-L")
    libs = native.libs |> Enum.map_join(" ", fn lib -> "-l#{lib}" end)
    pkg_config_libs = pkg_config(native, :libs)
    "#{pkg_config_libs} #{lib_dirs} #{libs}"
  end

  defp pkg_config(%Native{pkg_configs: []}, _options), do: ""

  defp pkg_config(%Native{pkg_configs: packages, app: app}, options) do
    options = options |> Bunch.listify() |> Enum.map(&"--#{&1}")
    System.put_env("PATH", System.get_env("PATH", "") <> ":/usr/local/bin:/opt/homebrew/bin")

    case System.cmd("which", ["pkg-config"]) do
      {_path, 0} ->
        :ok

      {_path, _error} ->
        Output.raise("""
        pkg-config not found. Bundlex needs pkg-config to find packages in system.
        On Mac OS, you can install pkg-config via Homebrew by typing `brew install pkg-config`.
        """)
    end

    Enum.map_join(packages, " ", fn package ->
      case System.cmd("pkg-config", options ++ [package], stderr_to_stdout: true) do
        {output, 0} ->
          String.trim_trailing(output)

        {output, error} ->
          Output.raise("""
          Couldn't find system package #{package} with pkg-config. Check whether it's installed.
          Installation instructions may be available in the readme of package #{app}.
          Output from pkg-config:
          Error: #{error}
          #{output}
          """)
      end
    end)
  end
end
