defmodule TestBrowser.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_browser,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {TestBrowser.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:cowlib, "~> 2.11", override: true},
      {:stampede, github: "membraneframework-labs/stampede-elixir"},
    ]
  end
end
