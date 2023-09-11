defmodule Membrane.Common.C.Mixfile do
  use Mix.Project

  @version "0.14.0"
  @github_url "https://github.com/membraneframework/membrane-common-c"

  def project do
    [
      app: :membrane_common_c,
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:unifex, :bundlex] ++ Mix.compilers(),
      deps: deps(),

      # hex
      description: "Membrane Multimedia Framework (C language common routines)",
      package: package(),

      # docs
      name: "Membrane: Common C",
      source_url: @github_url,
      docs: docs()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache-2.0"],
      files: ["lib", "c_src", "mix.exs", "README*", "LICENSE*", ".formatter.exs", "bundlex.exs"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membraneframework.org"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md"],
      formatters: ["html"],
      source_ref: "v#{@version}"
    ]
  end

  defp deps() do
    [
      {:ex_doc, "~> 0.28", only: :dev, runtime: false},
      {:membrane_core, "~> 0.11.0"},
      {:shmex, "~> 0.5.0"},
      {:unifex, "~> 1.0"}
    ]
  end
end
