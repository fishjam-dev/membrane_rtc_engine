defmodule Membrane.RTP.Opus.Mixfile do
  use Mix.Project

  @version "0.7.0"
  @github_url "https://github.com/membraneframework/membrane_rtp_opus_plugin"

  def project do
    [
      app: :membrane_rtp_opus_plugin,
      version: @version,
      elixir: "~> 1.9",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "Membrane Multimedia Framework (RTP Opus)",
      package: package(),

      # docs
      name: "Membrane: RTP Opus",
      source_url: @github_url,
      homepage_url: "https://membraneframework.org",
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [],
      mod: {Membrane.RTP.Opus.Plugin.App, []}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:membrane_core, "~> 0.11.2"},
      {:membrane_rtp_format, "~> 0.6.0"},
      {:membrane_opus_format, "~> 0.3.0"},
      {:ex_doc, "~> 0.24.0", only: :dev, runtime: false},
      {:credo, "~> 1.6.1", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false}
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
      extras: ["LICENSE", "README.md"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.RTP.Opus]
    ]
  end
end
