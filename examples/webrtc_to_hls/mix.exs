defmodule WebRTCToHLS.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_webrtc_to_hls_demo,
      version: "0.1.0",
      elixir: "~> 1.14",
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
      {:elixir_uuid, "~> 1.2"},
      {:esbuild, "~> 0.4", runtime: Mix.env() == :dev},
      {:cowlib, "~> 2.11.0", override: true},

      # rtc engine dependencies
      {:membrane_rtc_engine, "~> 0.22.0"},
      {:membrane_rtc_engine_hls, "~> 0.7.0"},
      {:membrane_rtc_engine_webrtc, "~> 0.8.0"},

      # hls dependencies
      {:membrane_audio_mix_plugin, "~> 0.16.0", optional: true},
      {:membrane_video_compositor_plugin, "~> 0.7.0", optional: true}
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
