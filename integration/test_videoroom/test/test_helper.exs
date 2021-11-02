ExUnit.start()

{:ok, _} = Application.ensure_all_started(:wallaby)

Application.put_env(:wallaby, :base_url, "http://localhost:4001")
Application.put_env(:wallaby, :screenshot_on_failure, true)
# don't throw on js errors
# Application.put_env(:wallaby, :js_errors, false)

# log js related errors to file
{:ok, file} = File.open("browser_logs.log", [:write])
Application.put_env(:wallaby, :js_logger, file)
