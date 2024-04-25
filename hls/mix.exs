defmodule Membrane.RTC.Engine.Endpoint.HLS.MixProject do
  use Mix.Project

  @version "0.8.0-dev"
  @engine_github_url "https://github.com/jellyfish-dev/membrane_rtc_engine"
  @github_url "#{@engine_github_url}/tree/master/hls"
  @source_ref "hls-v#{@version}"

  def project do
    [
      app: :membrane_rtc_engine_hls,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "HLS Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine HLS Endpoint",
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
      ],

      # dialyzer
      # this is because of optional dependencies
      # they are not included in PLT
      dialyzer: [
        plt_add_apps: [
          :membrane_video_compositor_plugin
        ]
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
      {:membrane_core, "~> 1.0"},
      {:membrane_aac_plugin, "~> 0.18.0"},
      {:membrane_opus_plugin, "~> 0.19.3"},
      {:membrane_aac_fdk_plugin, "~> 0.18.5"},
      {:membrane_raw_audio_format, "~> 0.12.0"},
      {:membrane_raw_video_format, "~> 0.3.0"},
      {:membrane_h26x_plugin, "~> 0.10.0"},
      {:membrane_h264_ffmpeg_plugin, "~> 0.31.0"},
      {:membrane_http_adaptive_stream_plugin, "~> 0.18.2"},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},

      # Optional deps for mixing audio and composing video
      {:membrane_audio_mix_plugin, "~> 0.16.0", optional: true},
      {:membrane_video_compositor_plugin, "~> 0.7.0", optional: true},

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
      source_url_pattern: "#{@engine_github_url}/blob/#{@source_ref}/hls/%{path}#L%{line}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end
end
