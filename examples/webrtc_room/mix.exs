defmodule WebRTCRoom.MixProject do
  use Mix.Project

  def project do
    [
      app: :webrtc_room,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {WebRTCRoom, []}
    ]
  end

  defp deps do
    [
      {:membrane_rtc_engine, path: "../../engine"},
      {:membrane_rtc_engine_webrtc, path: "../../webrtc"},
      {:plug, "~> 1.15.0"},
      {:bandit, "~> 1.2.0"},
      {:websock_adapter, "~> 0.5.0"},
      {:jason, "~> 1.4.0"}
    ]
  end
end
