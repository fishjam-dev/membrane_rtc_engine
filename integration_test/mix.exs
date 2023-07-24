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
      {:membrane_rtc_engine, path: "../membrane_rtc_engine", override: true},
      {:membrane_rtc_engine_hls, path: "../membrane_rtc_engine_hls"},
      {:membrane_rtc_engine_rtsp, path: "../membrane_rtc_engine_rtsp"},

      # Test deps
      {:excoveralls, "~> 0.16.0", only: :test, runtime: false}
    ]
  end
end
