ExUnit.start()

{:ok, _} = Application.ensure_all_started(:wallaby)

Application.put_env(:wallaby, :base_url, "http://localhost:4001")
Application.put_env(:wallaby, :screenshot_on_failure, true)
# don't throw on js errors
# Application.put_env(:wallaby, :js_errors, false)

# log js related errors to file
{:ok, file} = File.open("browser_logs.log", [:write])
Application.put_env(:wallaby, :js_logger, file)

Application.put_env(:wallaby, :chromedriver,
  capabilities: %{
    javascriptEnabled: true,
    loadImages: true,
    version: "",
    rotatable: false,
    takesScreenshot: true,
    cssSelectorsEnabled: true,
    nativeEvents: false,
    platform: "ANY",
    unhandledPromptBehavior: "accept",
    loggingPrefs: %{
      browser: "DEBUG"
    },
    chromeOptions: %{args: ["--headless", "--use-fake-ui-for-media-stream"]}
    },
  headless: true
)

{:ok, _pid} = Node.start(:test_node, :shortnames)
