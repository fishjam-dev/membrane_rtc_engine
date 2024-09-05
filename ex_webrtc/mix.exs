defmodule Membrane.RTC.Engine.Endpoint.ExWebRTC.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_rtc_engine_ex_webrtc,
      version: "0.1.0",
      elixir: "~> 1.14",
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
      {:membrane_rtc_engine, path: "../engine"},
      {:membrane_core, "~> 1.0"},
      {:ex_webrtc, github: "elixir-webrtc/ex_webrtc", branch: "simulcast"},
      {:ex_sdp, "~> 0.17.0", override: true},
    ]
  end
end
