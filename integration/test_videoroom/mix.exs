defmodule TestVideoroom.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_videoroom,
      version: "0.1.0",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {TestVideoroom.Application, []},
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.6.2"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_view, "~> 0.17"},
      {:esbuild, "~> 0.4", runtime: Mix.env() == :dev},
      {:telemetry, "~> 1.0", override: true},
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.5"},
      {:cowlib, "~> 2.11", override: true},
      {:membrane_rtc_engine, path: "../../"},
      {:stampede, github: "geometerio/stampede-elixir"},
      {:membrane_webrtc_plugin, path: "../../deps/membrane_webrtc_plugin", override: true}
    ]
  end
end
