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
    Enum.each(options.buttons, fn
      {{button, timeout}, stage}
      when button in [
             "simulcast-local-low-encoding",
             "simulcast-local-medium-encoding",
             "simulcast-local-high-encoding"
           ] ->
        get_stats(
          page,
          options.receiver,
          options.id,
          stage,
          options.simulcast_outbound_stats_button
        )

        Process.sleep(1_000)

        :ok = Playwright.Page.click(page, "[id=#{button}]")

        get_stats_periodically(
          options.simulcast_outbound_stats_button,
          page,
          options,
          stage,
          timeout - 2_000
        )

      {{button, timeout}, stage}
      when button in [
             "simulcast-peer-low-encoding",
             "simulcast-peer-medium-encoding",
             "simulcast-peer-high-encoding"
           ] ->
        get_stats(
          page,
          options.receiver,
          options.id,
          stage,
          options.simulcast_inbound_stats_button
        )

        Process.sleep(1_000)

        :ok = Playwright.Page.click(page, "[id=#{button}]")

        get_stats_periodically(
          options.simulcast_inbound_stats_button,
          page,
          options,
          stage,
          timeout - 2_000
        )

      {{button, timeout}, stage}
      when button in ["simulcast-inbound-stats", "simulcast-outbound-stats"] ->
        get_stats_periodically(button, page, options, stage, timeout)

      {{button, timeout}, stage} ->
        :ok = Playwright.Page.click(page, "[id=#{button}]")
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

  defp get_stats_periodically(button, page, options, stage, timeout) do
    repeats = div(timeout, 2_000)

    timeouts = List.duplicate(1_000, repeats)

    for timeout <- timeouts do
      get_stats(page, options.receiver, options.id, stage, button)
      Process.sleep(timeout)
    end
  end
end
