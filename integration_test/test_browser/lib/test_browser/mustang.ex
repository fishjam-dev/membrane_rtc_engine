defmodule TestBrowser.Mustang do
  use Stampede.Mustang
  require Logger

  @impl true
  def join(browser, options) do
    page = browser |> Playwright.Browser.new_page()
    _response = Playwright.Page.goto(page, options.target_url)
    Playwright.Page.on(page, :console, fn msg -> Logger.info("Browser console: #{msg.params.message.message_text}") end)

    page
    |> Playwright.Page.locator("[id=#{options.start_button}]")
    |> Playwright.Locator.click()

    {browser, page}
  end

  @impl true
  def afterJoin({browser, page}, options) do
    Process.sleep(options.warmup_time)

    {browser, page}
  end

  @impl true
  def linger({_browser, page} = ctx, options) do
    Enum.each(options.actions, fn
      {:click, button, timeout} = action ->
        Logger.info("mustang: #{options.id}, action: #{inspect(action)}")
        :ok = Playwright.Page.click(page, "[id=#{button}]")
        Process.sleep(timeout)

      {:get_stats, button, repeats, timeout, tag: tag} = action ->
        Logger.info("mustang: #{options.id}, action: #{inspect(action)}")
        timeouts = List.duplicate(timeout, repeats)

        for timeout <- timeouts do
          get_stats(page, options.receiver, options.id, tag, button)
          Process.sleep(timeout)
        end

      {:wait, timeout} = action ->
        Logger.info("mustang: #{options.id}, action: #{inspect(action)}")
        Process.sleep(timeout)

      {:notify_server, msg} = action ->
        Logger.info("mustang: #{options.id}, action: #{inspect(action)}")
        send(options.server, {msg, options.id})

    end)

    ctx
  end

  @impl true
  def beforeLeave(ctx, _options) do
    ctx
  end

  @impl true
  def leave({browser, page}, options) do
    Playwright.Page.click(page, "[id=stop]")
    send(options.receiver, {options.id, :end})
    Playwright.Page.close(page)
    Playwright.Browser.close(browser)

    browser
  end

  defp get_stats(page, receiver, browser_id, tag, button) do
    :ok = Playwright.Page.click(page, "[id=#{button}]")
    Process.sleep(1_000)

    page
    |> Playwright.Page.text_content("[id=data]")
    |> case do
      "uninitialized" ->
        {:error, :uninitialized}

      "Room error." <> reason ->
        {:error, reason}

      data ->
        Jason.decode!(data)
    end
    |> then(fn data -> send(receiver, {browser_id, tag, data}) end)
  end
end
