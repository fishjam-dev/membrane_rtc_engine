defmodule Qex.Mixfile do
  use Mix.Project

  @source_url "https://github.com/princemaple/elixir-queue"
  @version "0.5.1"

  def project do
    [
      app: :qex,
      name: "Qex",
      version: @version,
      elixir: "~> 1.9",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [{:ex_doc, ">= 0.0.0", only: :docs, runtime: false}]
  end

  defp package do
    [
      description:
        "A `:queue` wrapper with improvements in API and addition of Protocol implementations",
      licenses: ["MIT"],
      maintainers: ["Po Chen"],
      links: %{
        Changelog: "https://hexdocs.pm/qex/changelog.html",
        GitHub: @source_url
      }
    ]
  end

  defp docs do
    [
      extras: [
        "CHANGELOG.md": [],
        "LICENSE.md": [title: "License"],
        "README.md": [title: "Overview"]
      ],
      main: "readme",
      canonical: "http://hexdocs.pm/qex",
      homepage_url: @source_url,
      source_url: @source_url,
      source_ref: "v#{@version}"
    ]
  end
end
