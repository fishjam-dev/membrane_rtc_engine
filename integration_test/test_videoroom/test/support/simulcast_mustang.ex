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

    {browser, page}
  end

  @impl true
  def linger({_browser, page} = ctx, options) do
    Enum.each(options.actions, fn
      {:click, button, repeats, timeout, tag: tag}
      when button in [
             "simulcast-local-low-encoding",
             "simulcast-local-medium-encoding",
             "simulcast-local-high-encoding",
             "simulcast-peer-low-encoding",
             "simulcast-peer-medium-encoding",
             "simulcast-peer-high-encoding"
           ] ->
        timeouts = List.duplicate(timeout, repeats)

        for timeout <- timeouts do
          get_stats(page, options.receiver, options.id, tag, button)
          Process.sleep(timeout)
        end

      {:click, button, repeats, timeout, tag: _tag}
      when button in ["simulcast-inbound-stats", "simulcast-outbound-stats"] ->
        timeouts = List.duplicate(timeout, repeats)

        for timeout <- timeouts do
          :ok = Playwright.Page.click(page, "[id=#{button}]")
          Process.sleep(timeout)
        end

      {:wait, timeout} ->
        Process.sleep(timeout)
    end)

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
end
