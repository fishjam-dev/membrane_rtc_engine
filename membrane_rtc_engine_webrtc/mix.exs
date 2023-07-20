defmodule Membrane.RTC.Engine.Endpoint.WebRTC.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github_url "https://github.com/jellyfish-dev/membrane_rtc_engine/tree/master/membrane_rtc_engine_webrtc"

  def project do
    [
      app: :membrane_rtc_engine_webrtc,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "WebRTC Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine WebRTC Endpoint",
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
      # Engine deps
      {:membrane_rtc_engine, path: "../membrane_rtc_engine"},

      # Regular deps
      {:membrane_core, "~> 0.12.3"},
      {:membrane_webrtc_plugin, "~> 0.15.0"},
      {:membrane_rtp_plugin, "~> 0.23.0"},
      {:membrane_ice_plugin, "~> 0.16.0"},
      {:membrane_rtp_format, "~> 0.7.0"},
      {:membrane_opentelemetry, "~> 0.1.0"},
      {:membrane_rtp_vp8_plugin, "~> 0.8.0"},
      {:membrane_rtp_h264_plugin, "~> 0.16.0"},
      {:membrane_telemetry_metrics, "~> 0.1.0"},
      {:ex_sdp, "~> 0.11.0"},
      {:qex, "~> 0.5"},
      {:jason, "~> 1.2"},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},

      # Test deps
      {:membrane_file_plugin, "~> 0.14.0"},
      {:excoveralls, "~> 0.16.0", only: :test, runtime: false},
      {:membrane_realtimer_plugin, "~> 0.6.1", only: :test},

      # Otel
      {:opentelemetry_api, "~> 1.0.0"}
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
      extras: extras(),
      formatters: ["html"],
      groups_for_extras: groups_for_extras(),
      source_ref: "webrtc-v#{@version}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end

  defp extras() do
    [
      "README.md",
      "LICENSE",
      # guides
      "../../guides/simulcast.md",

      # internal docs
      "../../internal_docs/webrtc_media_events.md",
      "../../internal_docs/protocol.md",
      "../../internal_docs/webrtc_endpoint.md",
      "../../internal_docs/simulcast.md": [filename: "internal_simulcast"]
    ]
  end

  defp groups_for_extras() do
    [
      {"Developer docs", ~r/internal_docs\//},
      # negative lookahead to match everything
      # except upgrading directory
      {"Guides", ~r/guides\/^(.(?!upgrading\/))*$/}
    ]
  end
end
