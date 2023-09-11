defmodule Membrane.TelemetryMetrics.Mixfile do
  use Mix.Project

  @version "0.1.0"
  @github_url "https://github.com/membraneframework/membrane_telemetry_metrics"

  def project do
    [
      app: :membrane_telemetry_metrics,
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: dialyzer(),

      # hex
      description: "TelemetryMetrics for Membrane Multimedia Framework",
      package: package(),

      # docs
      name: "Membrane Telemetry Metrics",
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
      {:telemetry, "~> 1.0"},
      {:telemetry_metrics, "~> 0.6.1"},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false},
      {:credo, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end

  defp dialyzer() do
    opts = [
      plt_local_path: "priv/plts",
      flags: [:error_handling]
    ]

    if System.get_env("CI") == "true" do
      # Store core PLTs in cacheable directory for CI
      # For development it's better to stick to default, $MIX_HOME based path
      # to allow sharing core PLTs between projects
      [plt_core_path: "priv/plts"] ++ opts
    else
      opts
    end
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
      formatters: ["html"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.TelemetryMetrics]
    ]
  end
end
