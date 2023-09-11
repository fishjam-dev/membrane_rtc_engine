defmodule Membrane.ICE.Mixfile do
  use Mix.Project

  @version "0.15.1"
  @github_url "https://github.com/jellyfish-dev/membrane_ice_plugin"

  def project do
    [
      app: :membrane_ice_plugin,
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: dialyzer(),

      # hex
      description: "Plugin for managing ICE connection",
      package: package(),

      # docs
      name: "Membrane ICE plugin",
      source_url: @github_url,
      homepage_url: "https://membrane.stream",
      docs: docs()
    ]
  end

  def application do
    [
      mod: {Membrane.ICE.Application, []},
      extra_applications: []
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:membrane_opentelemetry, "~> 0.1.0"},
      {:membrane_core, "~> 0.11"},
      {:membrane_rtp_format, "~> 0.6.0"},
      {:membrane_funnel_plugin, "~> 0.7.0"},
      {:membrane_telemetry_metrics, "~> 0.1.0"},
      {:bunch, "~> 1.5"},
      {:fake_turn, "~> 0.4.0"},
      {:ex_dtls, "~> 0.12.0"},
      {:ex_doc, "~> 0.26", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},
      {:credo, "~> 1.6", only: :dev, runtime: false}
    ]
  end

  defp dialyzer do
    opts = [
      flags: [:error_handling]
    ]

    if System.get_env("CI") == "true" do
      # Store PLTs in cacheable directory for CI
      [plt_local_path: "priv/plts", plt_core_path: "priv/plts"] ++ opts
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
        "Membrane Framework Homepage" => "https://membrane.stream"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "LICENSE"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.ICE],
      formatters: ["html"]
    ]
  end
end
