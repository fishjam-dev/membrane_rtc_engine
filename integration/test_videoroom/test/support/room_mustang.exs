defmodule RoomMustang do
  use Stampede.Mustang

  @impl true
  def join(browser, options) do
    page = browser |> Playwright.Browser.new_page()
    _response = Playwright.Page.goto(page, options.target_url)

    Process.sleep(500)

    :ok = Playwright.Page.click(page, "[id=start-none]")

    {browser, page}
  end

  @impl true
  def linger({_browser, _page} = ctx, options) do
    ctx
  end

  @impl true
  def leave({browser, page}, options) do
    :ok = Playwright.Page.click(page, "[id=stop]")
    send(options.receiver, {options.id, :end})
    Playwright.Page.close(page)
    Playwright.Browser.close(browser)

    browser
  end
end
