defmodule ExampleMustang do
  use Stampede.Mustang

  @impl true
  def join(browser, options) do
    page = browser |> Playwright.Browser.new_page()

    page |> Playwright.Page.goto(options.target_url)

    browser =
    page
    |> Playwright.Page.fill("[name=display_name]", "stampede")
    |> Playwright.Page.click("[type=submit]")

    {browser, page}
  end

  @impl true
  def linger({_browser, _page} = ctx, _options) do
    :timer.sleep(:timer.seconds(10))
    ctx
  end

  @impl true
  def leave({browser, page}, _options) do
    Playwright.Page.click(page, "[id=disconnect]")
    browser
  end

end
