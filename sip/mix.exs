defmodule Membrane.RTC.Engine.Endpoint.SIP.MixProject do
  use Mix.Project

  @version "0.0.1-dev"
  @engine_github_url "https://github.com/jellyfish-dev/membrane_rtc_engine"
  @github_url "#{@engine_github_url}/tree/master/sip"
  @source_ref "sip-v#{@version}"

  def project do
    [
      app: :membrane_rtc_engine_sip,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "SIP Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine SIP Endpoint",
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
      {:membrane_rtc_engine, path: "../engine"},
      {:membrane_rtc_engine_webrtc, path: "../webrtc"},

      # Regular deps
      {:membrane_core, "~> 0.12.3"},
      {:membrane_rtp_plugin, "~> 0.23.0"},
      {:membrane_rtp_format, "~> 0.7.0"},
      {:membrane_udp_plugin, "~> 0.10.0"},
      {:sippet, "~> 1.0"},
      {:uuid, "~> 1.1"},
      {:ex_sdp, "~> 0.11"},
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
      extras: ["README.md", "CHANGELOG.md", "LICENSE"],
      formatters: ["html"],
      source_ref: @source_ref,
      source_url_pattern: "#{@engine_github_url}/blob/#{@source_ref}/sip/%{path}#L%{line}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end
end
