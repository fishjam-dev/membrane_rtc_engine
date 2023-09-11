defmodule Bundlex.Mixfile do
  use Mix.Project

  @version "1.1.1"
  @github_url "https://github.com/membraneframework/bundlex"

  def project do
    [
      app: :bundlex,
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: dialyzer(),

      # hex
      description: "Multi-Platform build system for Elixir",
      package: package(),

      # docs
      name: "Bundlex",
      source_url: @github_url,
      homepage_url: "https://membraneframework.org",
      docs: docs()
    ]
  end

  def application do
    [extra_applications: [:logger], mod: {Bundlex.App, []}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

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
      extras: ["README.md", "LICENSE"],
      formatters: ["html"],
      source_ref: "v#{@version}"
    ]
  end

  defp dialyzer() do
    opts = [
      flags: [:error_handling],
      plt_add_apps: [:mix]
    ]

    if System.get_env("CI") == "true" do
      # Store PLTs in cacheable directory for CI
      [plt_local_path: "priv/plts", plt_core_path: "priv/plts"] ++ opts
    else
      opts
    end
  end

  defp deps() do
    [
      {:bunch, "~> 1.0"},
      {:qex, "~> 0.5"},
      {:secure_random, "~> 0.5"},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false},
      {:credo, "~> 1.6", only: :dev, runtime: false}
    ]
  end
end
