defmodule Membrane.RTC.Engine.Integration.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_rtc_engine_integration_test,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # test coverage
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.json": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      # Engine deps
      {:membrane_rtc_engine, path: "../engine", override: true},
      {:membrane_rtc_engine_hls, path: "../hls"},
      {:membrane_rtc_engine_rtsp, path: "../rtsp"},

      # Regular deps (for modules in `test/support/`)
      {:membrane_core, "~> 0.12.7"},
      {:membrane_rtp_plugin, "~> 0.23.0"},
      {:membrane_rtp_h264_plugin, "~> 0.18.0"},
      {:membrane_h264_format, "~> 0.6.0"},
      {:membrane_h264_plugin, "~> 0.7.2"},
      {:membrane_udp_plugin, "~> 0.10.0"},
      {:membrane_file_plugin, "~> 0.15.0"},
      {:qex, "~> 0.5.1"},

      # Test deps
      {:excoveralls, "~> 0.16.0", only: :test, runtime: false}
    ]
  end
end
