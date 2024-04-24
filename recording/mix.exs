defmodule Recording.MixProject do
  use Mix.Project

  @version "0.1.0"
  @engine_github_url "https://github.com/jellyfish-dev/membrane_rtc_engine"
  @github_url "#{@engine_github_url}/tree/master/recording"
  @source_ref "recording-v#{@version}"

  def project do
    [
      app: :membrane_rtc_engine_recording,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # hex
      description: "Recording Endpoint for Membrane RTC Engine",
      package: package(),

      # docs
      name: "Membrane RTC Engine Recording Endpoint",
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
      mod: {Membrane.RTC.Engine.Endpoint.Recording.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:elixir_uuid, "~> 1.2"},

      # Engine deps
      {:membrane_rtc_engine, "~> 0.22.0"},
      {:membrane_rtc_engine_webrtc, "~> 0.8.0"},

      # Membrane deps
      {:membrane_core, "~> 1.0"},
      {:membrane_stream_plugin, "~> 0.4.0"},
      {:membrane_tee_plugin, "~> 0.12.0"},
      {:membrane_file_plugin, "~> 0.16.0"},

      # AWS deps
      {:ex_aws, "~> 2.1"},
      {:ex_aws_s3, "~> 2.0"},
      {:hackney, "~> 1.9"},
      {:sweet_xml, "~> 0.6"},

      # lint
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: :dev, runtime: false},

      # Test deps
      {:excoveralls, "~> 0.16.0", only: :test, runtime: false},
      {:mox, "~> 1.0", only: :test}
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
      source_url_pattern: "#{@engine_github_url}/blob/#{@source_ref}/recording/%{path}#L%{line}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint]
    ]
  end
end
