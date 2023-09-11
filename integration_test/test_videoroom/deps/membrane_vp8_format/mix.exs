defmodule Membrane.VP8.Mixfile do
  use Mix.Project

  @version "0.4.0"
  @github_url "https://github.com/membraneframework/membrane_vp8_format"

  def project do
    [
      app: :membrane_vp8_format,
      version: @version,
      elixir: "~> 1.10",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "Membrane Multimedia Framework (VP8 Format Description)",
      package: package(),

      # docs
      name: "Membrane: VP8 Format",
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

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:ex_doc, "~> 0.22", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1.0", only: :dev, runtime: false},
      {:credo, "~> 1.4", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membraneframework.org"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "LICENSE"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.VP8]
    ]
  end
end
