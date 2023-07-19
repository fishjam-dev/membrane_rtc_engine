defmodule Membrane.RTC.Engine.Endpoint.RTSP.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github_url "https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/endpoints/rtsp"

  def project do
    [
      app: :membrane_rtc_engine_endpoint_rtsp,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "RTSP Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine RTSP Endpoint",
      source_url: @github_url,
      homepage_url: "https://membrane.stream",
      docs: docs(),

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
      {:membrane_rtc_engine, path: "../.."},
      {:membrane_rtc_engine_endpoint_webrtc, path: "../webrtc"},
      {:membrane_core, "~> 0.12.3"},
      {:membrane_rtp_plugin, "~> 0.23.0"},
      {:membrane_rtp_format, "~> 0.7.0"},
      {:membrane_rtp_h264_plugin, "~> 0.16.0"},
      {:ex_sdp, "~> 0.11.0"},
      {:connection, "~> 1.1"},
      {:membrane_rtsp, "~> 0.5.0"},
      {:membrane_udp_plugin, "~> 0.10.0"},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},

      # Test deps
      {:excoveralls, "~> 0.16.0", only: :test, runtime: false}
    ]
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membrane.stream"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "LICENSE"],
      formatters: ["html"],
      source_ref: "rtsp-v#{@version}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end
end
