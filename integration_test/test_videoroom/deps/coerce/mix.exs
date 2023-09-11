defmodule Coerce.Mixfile do
  use Mix.Project

  def project do
    [
      app: :coerce,
      version: "1.0.1",
      elixir: "~> 1.3",
      start_permanent: Mix.env == :prod,
      build_embedded: Mix.env == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      name: "Coerce",
      source_url: "https://gitub.com/Qqwy/elixir-coerce"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},

      {:dialyxir, "~> 0.3", only: :dev},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp description do
    """
    Coerce allows defining coercions, standardized conversions, between data types.
    """
  end

  defp package do
    [# These are the default files included in the package
      name: :coerce,
      files: ["lib", "mix.exs", "README*"],
      maintainers: ["Wiebe-Marten/Qqwy"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/qqwy/elixir-coerce",
      }]
  end
end
