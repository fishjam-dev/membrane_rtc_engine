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

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {TestVideoroom.Application, []},
      extra_applications: [:logger, :runtime_tools]
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
      {:esbuild, "~> 0.2", runtime: Mix.env() == :dev},
      {:telemetry, "~> 1.0", override: true},
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.5"},
      # TODO: change to versioned package once a PR fixing http client headers are merged
      # {:wallaby, "~> 0.29.0", runtime: false, only: :test},
      {:wallaby,
       github: "Qizot/wallaby", branch: "fix-http-client-headers", runtime: false, only: :test},
      {:membrane_rtc_engine, path: "../../"}
    ]
  end
end
