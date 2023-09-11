defmodule Membrane.H264.FFmpeg.Plugin.MixProject do
  use Mix.Project

  @version "0.26.2"
  @github_url "https://github.com/membraneframework/membrane_h264_ffmpeg_plugin"

  def project do
    [
      app: :membrane_h264_ffmpeg_plugin,
      compilers: [:unifex, :bundlex] ++ Mix.compilers(),
      version: @version,
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: dialyzer(),

      # Hex
      description: "Membrane H264 parser, decoder and encoder based on FFmpeg and x264",
      package: package(),

      # Docs
      name: "Membrane H264 FFmpeg plugin",
      source_url: @github_url,
      homepage_url: "https://membraneframework.org",
      docs: docs()
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
      {:bunch, "~> 1.6"},
      {:unifex, "~> 1.1"},
      {:membrane_core, "~> 0.11.0"},
      {:membrane_common_c, "~> 0.14.0"},
      {:membrane_h264_format, "~> 0.5.0"},
      {:membrane_raw_video_format, "~> 0.3.0"},
      {:ratio, "~> 2.4.0"},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},
      {:membrane_raw_video_parser_plugin, "~> 0.10.0", only: :test},
      {:membrane_file_plugin, "~> 0.13.0", only: :test}
    ]
  end

  defp dialyzer() do
    opts = [
      flags: [:error_handling]
    ]

    if System.get_env("CI") == "true" do
      # Store PLTs in cacheable directory for CI
      File.mkdir_p!(Path.join([__DIR__, "priv", "plts"]))
      [plt_local_path: "priv/plts", plt_core_path: "priv/plts"] ++ opts
    else
      opts
    end
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md"],
      formatters: ["html"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [
        Membrane.H264.FFmpeg,
        Membrane.H264.FFmpeg.Parser
      ],
      groups_for_modules: [
        Parser: [~r/^Membrane\.H264\.FFmpeg\.Parser\.DecoderConfiguration$/]
      ]
    ]
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membraneframework.org"
      },
      files: ["lib", "mix.exs", "README*", "LICENSE*", ".formatter.exs", "bundlex.exs", "c_src"],
      exclude_patterns: [~r"c_src/.*/_generated.*"]
    ]
  end
end
