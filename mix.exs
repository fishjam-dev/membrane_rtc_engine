defmodule Membrane.SFU.MixProject do
  use Mix.Project

  def project do
    [
      app: :membrane_sfu,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      mod: {Membrane.SFUApp, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:membrane_core, "~> 0.7.0", override: true},
      {:membrane_webrtc_plugin, github: "membraneframework/membrane_webrtc_plugin"},
      {:membrane_element_tee, "~> 0.4.1"},
      {:membrane_element_fake, "~> 0.4.0"},
      {:jason, "~> 1.2"},
      {:dialyxir, "1.1.0", only: :dev, runtime: false},
      {:ex_doc, "0.24.2", only: :dev, runtime: false}
    ]
  end

  defp aliases() do
    [
      compile: ["compile", &compile_ts/1],
      docs: ["docs", &generate_ts_docs/1]
    ]
  end

  defp compile_ts(_) do
    Mix.shell().info("Installing npm dependencies")
    {result, exit_status} = System.cmd("npm", ["install"], cd: "assets")
    Mix.shell().info(result)
    if exit_status != 0, do: raise("Failed to install npm dependecies")

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
end
