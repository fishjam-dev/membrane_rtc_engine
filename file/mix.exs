defmodule Membrane.RTC.Engine.Endpoint.File.MixProject do
  use Mix.Project

  @version "0.5.0"
  @engine_github_url "https://github.com/jellyfish-dev/membrane_rtc_engine"
  @github_url "#{@engine_github_url}/tree/master/file"
  @source_ref "file-v#{@version}"

  def project do
    [
      app: :membrane_rtc_engine_file,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "File Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine File Endpoint",
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
      {:membrane_rtc_engine, "~> 0.22.0"},
      {:membrane_rtc_engine_webrtc, "~> 0.8.0"},

      # Regular deps
      {:membrane_core, "~> 1.0"},
      {:membrane_realtimer_plugin, "~> 0.9.0"},
      {:membrane_file_plugin, "~> 0.16.0"},
      {:membrane_opus_plugin, "~> 0.19.3"},
      {:membrane_ogg_plugin, "~> 0.3.0"},
      {:membrane_h26x_plugin, "~> 0.10.0"},
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},

      # Test deps,
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
      source_url_pattern: "#{@engine_github_url}/blob/#{@source_ref}/file/%{path}#L%{line}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end
end
