defmodule WebRTCToHLS.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_webrtc_to_hls_demo,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  def application do
    [
      mod: {WebRTCToHLS.Application, []},
      extra_applications: [:logger, :crypto]
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.5.2"},
      {:phoenix, "~> 1.6"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_view, "~> 0.16.0"},
      {:phoenix_live_reload, "~> 1.3"},
      {:jason, "~> 1.2"},
      {:phoenix_inline_svg, "~> 1.4"},
      {:uuid, "~> 1.1"},
      {:esbuild, "~> 0.4", runtime: Mix.env() == :dev},
      {:cowlib, "~> 2.11.0", override: true},

      # rtc engine dependencies
      {:membrane_rtc_engine, "~> 0.18.0"},
      {:membrane_rtc_engine_hls, "~> 0.3.0"},
      {:membrane_rtc_engine_webrtc, "~> 0.4.0"},

      # hls dependencies
      {:membrane_audio_mix_plugin, "~> 0.15.2", optional: true},
      {:membrane_video_compositor_plugin, "~> 0.5.1", optional: true}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "cmd --cd assets npm ci"],
      "assets.deploy": [
        "cmd --cd assets npm run deploy",
        "esbuild default --minify",
        "phx.digest"
      ]
    ]
  end
end
