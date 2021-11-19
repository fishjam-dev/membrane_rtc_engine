defmodule Spy.MixProject do
  use Mix.Project

  def project do
    [
      app: :spy,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp deps do
    [
      {:recon, "~> 2.5", only: :dev, runtime: false}
    ]
  end
end
