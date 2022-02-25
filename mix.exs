defmodule Membrane.RTC.Engine.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github_url "https://github.com/membraneframework/membrane_rtc_engine"

  def project do
    [
      app: :membrane_rtc_engine,
      version: @version,
      elixir: "~> 1.10",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),

      # hex
      description: "Membrane RTC Engine and its client library",
      package: package(),

      # docs
      name: "Membrane RTC Engine",
      source_url: @github_url,
      homepage_url: "https://membraneframework.org",
      docs: docs(),

      # dialyzer
      # this is because of optional dependencies
      # they are not included in PLT
      dialyzer: [
        plt_add_apps: [
          :ex_libnice,
          :ex_sdp,
          :membrane_rtp_plugin,
          :membrane_ice_plugin,
          :membrane_webrtc_plugin
        ]
      ]
    ]
  end

  def application do
    [
      mod: {Membrane.RTC.Engine.App, []},
      extra_applications: []
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:membrane_core, "~> 0.9.0", override: true},
      {:membrane_tee_plugin, "~> 0.8.0", override: true},
      {:membrane_webrtc_plugin,
       github: "membraneframework/membrane_webrtc_plugin", optional: true},
      {:membrane_http_adaptive_stream_plugin, "~> 0.5.0", optional: true},
      {:membrane_mp4_plugin, "~> 0.11.0", optional: true},
      {:membrane_aac_plugin, "~> 0.11.0", optional: true},
      {:membrane_aac_fdk_plugin, "~> 0.9.0", optional: true},
      {:uuid, "~> 1.1"},
      {:jason, "~> 1.2"},
      {:unifex, "~> 0.7.0", override: true},
      {:dialyxir, "1.1.0", only: :dev, runtime: false},
      {:ex_doc, "0.24.2", only: :dev, runtime: false},
      {:credo, "~> 1.4", only: :dev, runtime: false, override: true},

      # Otel
      {:opentelemetry_api, "~> 0.6.0"},
      {:opentelemetry, "~> 0.6.0"},
      {:open_telemetry_decorator, "~> 0.5.4"}
    ]
  end

  defp aliases() do
    [
      compile: ["compile", &compile_ts/1],
      docs: ["docs", &generate_ts_docs/1]
    ]
  end

  defp package do
    [
      maintainers: ["Membrane Team"],
      licenses: ["Apache 2.0"],
      links: %{
        "GitHub" => @github_url,
        "Membrane Framework Homepage" => "https://membraneframework.org"
      },
      files:
        ~w(lib mix.exs .formatter.exs LICENSE README.md) ++
          ~w(assets/js assets/package.json assets/package-lock.json assets/tsconfig.json assets/esbuild.js package.json)
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "LICENSE"],
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Membrane.RTC.Engine.Endpoint, Membrane.RTC.Engine.Message],
      groups_for_modules: [
        Endpoints: [
          Membrane.RTC.Engine.Endpoint.WebRTC,
          Membrane.RTC.Engine.Endpoint.HLS,
          Membrane.RTC.Engine.Endpoint.Distributed
        ],
        Messages: [
          Membrane.RTC.Engine.Message,
          Membrane.RTC.Engine.Message.MediaEvent,
          Membrane.RTC.Engine.Message.NewPeer,
          Membrane.RTC.Engine.Message.PeerLeft
        ]
      ]
    ]
  end

  defp compile_ts(_) do
    Mix.shell().info("Installing npm dependencies")

    if packages_installed?() do
      Mix.shell().info("* Already installed")
    else
      {result, exit_status} = System.cmd("npm", ["ci"], cd: "assets")
      Mix.shell().info(result)
      if exit_status != 0, do: raise("Failed to install npm dependecies")
    end

    Mix.shell().info("Compiling TS files")
    {result, exit_status} = System.cmd("npm", ["run", "build"], cd: "assets")
    Mix.shell().info(result)
    if exit_status != 0, do: raise("Failed to compile TS files")
  end

  defp generate_ts_docs(_) do
    Mix.shell().info("Generating TS docs")
    {result, exit_status} = System.cmd("npm", ["run", "docs"], cd: "assets")
    Mix.shell().info(result)
    if exit_status != 0, do: raise("Failed to generate TS docs")
  end

  defp packages_installed?() do
    System.cmd("npm", ["ls", "--prefix", "assets", "--prefer-offline"], stderr_to_stdout: true)
    |> case do
      {output, 0} ->
        missing =
          output
          |> String.split("\n")
          |> Enum.filter(&Regex.match?(~r/UNMET DEPENDENCY|empty/, &1))

        if length(missing) > 0,
          do: false,
          else: true

      {_output, _} ->
        false
    end
  end
end
