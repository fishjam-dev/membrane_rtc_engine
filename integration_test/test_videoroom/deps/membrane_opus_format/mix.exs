defmodule Membrane.Opus.Format.Mixfile do
  use Mix.Project

  @version "0.3.0"
  @github_url "https://github.com/membraneframework/membrane_opus_format"

  def project do
    [
      app: :membrane_opus_format,
      version: @version,
      elixir: "~> 1.10",
      deps: deps(),

      # hex
      description: "Membrane Multimedia Framework (Opus Audio Format Description)",
      package: package(),

      # docs
      name: "Membrane: Opus Format",
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

  defp deps do
    [
      {:ex_doc, "~> 0.21.0", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache 2.0"],
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
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.Opus]
    ]
  end
end
