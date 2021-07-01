defmodule Membrane.SFU.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_sfu,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:membrane_core, "~> 0.7.0", override: true},
      {:membrane_webrtc_plugin, github: "membraneframework/membrane_webrtc_plugin"},
      {:membrane_element_tee, "~> 0.4.1"},
      {:membrane_element_fake, "~> 0.4.0"},
      {:jason, "~> 1.2"},
      {:dialyxir, "1.1.0", only: :dev, runtime: false},
      {:ex_doc, "0.24.2", only: :dev, runtime: false}
    ]
  end
end
