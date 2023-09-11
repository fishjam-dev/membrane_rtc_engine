defmodule Membrane.RawVideo.Mixfile do
  use Mix.Project

  @version "0.3.0"
  @github_url "https://github.com/membraneframework/membrane_raw_video_format"

  def project do
    [
      app: :membrane_raw_video_format,
      version: @version,
      elixir: "~> 1.12",
      description:
        "Definition of raw (uncompressed) video format for Membrane Multimedia Framework",
      package: package(),
      dialyzer: dialyzer(),
      name: "Membrane: Raw video format",
      source_url: @github_url,
      docs: docs(),
      deps: deps()
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", LICENSE: [title: "License"]],
      formatters: ["html"],
      source_ref: "v#{@version}"
    ]
  end

  defp dialyzer() do
    opts = [
      flags: [:error_handling]
    ]

    if System.get_env("CI") == "true" do
      [plt_core_path: "priv/plts"] ++ opts
    else
      opts
    end
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membrane.stream/"
      }
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.28", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},
      {:credo, "~> 1.6", only: :dev, runtime: false}
    ]
  end
end
