defmodule Rational.Mixfile do
  use Mix.Project

  def project do
    [
      app: :ratio,
      version: "2.4.2",
      elixir: "~> 1.4",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: description(),
      source_url: "https://github.com/qqwy/elixir-rational"
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      extra_applications: [
        :logger
      ]
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      # Markdown, dependency of ex_doc
      {:earmark, ">= 1.0.0", only: [:dev]},
      # Documentation for Hex.pm
      {:ex_doc, "~> 0.20", only: [:dev]},
      # Generic arithmetic dispatching.
      {:numbers, "~> 5.2.0"},
      # If Decimal number support is required
      {:decimal, "~> 1.6 or ~> 2.0", optional: true}
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README*", "LICENSE*"],
      maintainers: ["Qqwy/WM"],
      licenses: ["MIT"],
      links: %{github: "https://github.com/qqwy/elixir-rational"}
    ]
  end

  defp description do
    """
      This library allows you to use Rational numbers in Elixir, to enable exact calculations with all numbers big and small.
    """
  end

  # Can be overridden to allow different float precisions.
  Application.put_env(:ratio, :max_float_to_rational_digits, 10)
end
