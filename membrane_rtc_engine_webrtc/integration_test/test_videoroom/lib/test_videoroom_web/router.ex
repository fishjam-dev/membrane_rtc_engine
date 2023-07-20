defmodule TestVideoroomWeb.Router do
  use TestVideoroomWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {TestVideoroomWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", TestVideoroomWeb do
    pipe_through :browser

    get "/", PageController, :index
  end
end
