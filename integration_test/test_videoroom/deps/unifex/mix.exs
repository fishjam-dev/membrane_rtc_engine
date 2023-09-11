defmodule Unifex.MixProject do
  use Mix.Project

  @version "1.1.0"
  @github_url "https://github.com/membraneframework/unifex"

  def project do
    [
      app: :unifex,
      version: @version,
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      compilers: [:bundlex] ++ Mix.compilers(),
      deps: deps(),
      dialyzer: dialyzer(),

      # hex
      description: "Tool for generating interfaces between native C code and Elixir",
      package: package(),

      # docs
      name: "Unifex",
      source_url: @github_url,
      homepage_url: "https://membraneframework.org",
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

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
      extras: [
        "README.md",
        "LICENSE",
        "pages/creating_unifex_natives.md",
        "pages/creating_unifex_nif.md",
        "pages/supported_types.md"
      ],
      formatters: ["html"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [
        Unifex.CodeGenerators,
        Unifex.CodeGenerator.BaseTypes
      ],
      groups_for_modules: [
        CodeGenerators: [~r/Unifex\.CodeGenerators\.*/],
        BaseTypes: [~r/Unifex\.CodeGenerator.BaseTypes\.*/]
      ]
    ]
  end

  defp dialyzer() do
    opts = [
      flags: [:error_handling],
      plt_add_apps: [:mix]
    ]

    if System.get_env("CI") == "true" do
      # Store PLTs in cacheable directory for CI
      [plt_local_path: "priv/plts", plt_core_path: "priv/plts"] ++ opts
    else
      opts
    end
  end

  defp deps do
    [
      {:bunch, "~> 1.0"},
      {:shmex, "~> 0.5.0"},
      {:bundlex, "~> 1.0"},
      {:ex_doc, "~> 0.25", only: :dev, runtime: false},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false}
    ]
  end
end
