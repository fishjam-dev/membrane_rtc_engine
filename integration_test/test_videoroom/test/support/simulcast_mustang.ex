defmodule SimulcastMustang do
  use Stampede.Mustang

  @impl true
  def join(browser, options) do
    page = browser |> Playwright.Browser.new_page()
    _response = Playwright.Page.goto(page, options.target_url)

    Process.sleep(500)

    :ok = Playwright.Page.click(page, "[id=#{options.start_button}]")

    {browser, page}
  end

  @impl true
  def afterJoin({browser, page}, options) do
    Process.sleep(options.join_interval)

    buttons = Map.get(options, :start_buttons, [])

    for button <- buttons do
      :ok = Playwright.Page.click(page, "[id=#{button}]")
    end

    {browser, page}
  end

  @impl true
  def linger({_browser, _page} = ctx, options) when options.buttons == [] do
    Process.sleep(options.linger)
    ctx
  end

  @impl true
  def linger({_browser, page} = ctx, options) do
    for {{button, timeout}, stage} <- options.buttons do
      case button do
        stats when stats in ["simulcast-own-low", "simulcast-own-medium", "simulcast-own-high"] ->
          get_stats(page, options.receiver, options.id, stage, options.sender_button)

          Process.sleep(1_000)

          :ok = Playwright.Page.click(page, "[id=#{button}]")

          measurments(options.sender_button, page, options, stage, timeout - 2_000)

        stats
        when stats in ["simulcast-other-low", "simulcast-other-medium", "simulcast-other-high"] ->
          get_stats(page, options.receiver, options.id, stage, options.receiver_button)

          Process.sleep(1_000)

          :ok = Playwright.Page.click(page, "[id=#{button}]")

          measurments(options.receiver_button, page, options, stage, timeout - 2_000)

        stats when stats in ["simulcast-inbound-stats", "simulcast-outbound-stats"] ->
          measurments(button, page, options, stage, timeout)

        stats when stats in ["metadata-track", "metadata-peer"] ->
          Process.sleep(timeout - 1_000)
          get_stats(page, options.receiver, options.id, stage, button)
          Process.sleep(1_000)

        _other ->
          :ok = Playwright.Page.click(page, "[id=#{button}]")
          Process.sleep(timeout)
      end
    end

    ctx
  end

  @impl true
  def beforeLeave(ctx, _options) do
    Process.sleep(10_000)
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

  defp get_stats(page, receiver, browser_id, stage, button) do
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
    |> then(fn data -> send(receiver, {browser_id, stage, data}) end)
  end

  defp measurments(button, page, options, stage, timeout) do
    repeats = div(timeout, 2_000)

    timeouts = List.duplicate(1_000, repeats)

    for timeout <- timeouts do
      get_stats(page, options.receiver, options.id, stage, button)
      Process.sleep(timeout)
    end
  end
end
