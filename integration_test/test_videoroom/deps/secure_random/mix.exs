defmodule SecureRandom.Mixfile do
  use Mix.Project

  def project do
    [app: :secure_random,
     version: "0.5.1",
     elixir: "~> 1.2",
     deps: deps(),
     description: "A convienance library based on Ruby's SecureRandom",
     package: package()
    ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger, :crypto]]
  end

  defp package do
    [maintainers: ["Patrick Robertson"],
     licenses: ["Apache 2.0"],
     links: %{github: "https://github.com/patricksrobertson/secure_random.ex"}]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  def deps do
    [{:earmark, "~> 0.1", only: :dev},
     {:ex_doc, "~> 0.11", only: :dev}]
  end
end
