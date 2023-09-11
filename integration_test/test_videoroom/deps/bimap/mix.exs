defmodule BiMap.Mixfile do
  use Mix.Project

  @version "1.3.0"
  @github "https://github.com/mkaput/elixir-bimap"

  def project do
    [
      app: :bimap,
      name: "BiMap",
      description: description(),
      version: @version,
      elixir: "~> 1.3",
      source_url: @github,
      docs: [
        source_ref: "v#{@version}",
        main: "readme",
        extras: ~w(README.md)
      ],
      deps: deps(),
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # Development dependencies
      {:ex_doc, "~> 0.26", only: :dev, runtime: false},
      {:stream_data, "~> 0.5.0", only: [:dev, :test]}
    ]
  end

  defp description() do
    "Elixir implementation of bidirectional map and multimap"
  end

  defp package() do
    [
      files: ~w(
        lib
        mix.exs
        README.md
        LICENSE.txt
      ),
      maintainers: ["Marek Kaput <marek.kaput@outlook.com>"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => @github,
        "Changelog" => "#{@github}/releases"
      }
    ]
  end
end
