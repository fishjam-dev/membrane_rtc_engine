defmodule IntegrationMustang do
  use Stampede.Mustang

  @impl true
  def join(browser, options) do
    page = browser |> Playwright.Browser.new_page()
    _response = Playwright.Page.goto(page, options.target_url)

    :ok = Playwright.Page.click(page, "[id=#{options.start_button}]")

    {browser, page}
  end

  @impl true
  def afterJoin({browser, page}, options) do
    Process.sleep(options.join_interval)
    get_stats(page, options.receiver, options.id, :after_join)
    {browser, page}
  end

  @impl true
  def linger({_browser, _page} = ctx, options) do
    Process.sleep(options.linger)
    ctx
  end

  @impl true
  def beforeLeave({browser, page}, options) do
    get_stats(page, options.receiver, options.id, :before_leave)

    {browser, page}
  end

  @impl true
  def leave({browser, page}, options) do
    :ok = Playwright.Page.click(page, "[id=stop]")
    send(options.receiver, {options.id, :end})
    Playwright.Page.close(page)

    browser
  end

  defp get_stats(page, receiver, browser_id, stage) do
    :ok = Playwright.Page.click(page, "[id=stats]")
    Process.sleep(750)

    page
    |> Playwright.Page.text_content("[id=data]")
    |> case do
      "uninitialized" -> {:error, :uninitialized}
      data -> Jason.decode!(data)
    end
    |> then(fn data -> send(receiver, {browser_id, stage, data}) end)
  end
end
