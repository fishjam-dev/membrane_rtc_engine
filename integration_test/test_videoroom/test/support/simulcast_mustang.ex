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
  def linger({_browser, _page} = ctx, options) when options.buttons == [] do
    Process.sleep(options.linger)
    ctx
  end

  @impl true
  def linger({_browser, page} = ctx, options) do
    for {{button, timeout}, stage} <- options.buttons do
      if button == "simulcast-stats" do
        Process.sleep(timeout)
        get_stats(page, options.receiver, options.id, stage)
      else
        :ok = Playwright.Page.click(page, "[id=#{button}]")
        Process.sleep(timeout)
      end
    end

    ctx
  end

  @impl true
  def beforeLeave(ctx, _options) do
    Process.sleep(1_000)
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

  defp get_stats(page, receiver, browser_id, stage) do
    :ok = Playwright.Page.click(page, "[id=simulcast-stats]")
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
