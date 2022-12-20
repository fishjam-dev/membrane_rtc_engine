defmodule Membrane.RTC.Engine.Support.MessageEndpoint do
  @moduledoc false

  # Simple endpoint that sends message it received to its owner.

  use Membrane.Bin

  require Membrane.Logger

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              owner: [
                spec: pid(),
                description: "Pid of owner of endpoint"
              ]

  @impl true
  def handle_init(_ctx, opts) do
    state = %{rtc_engine: opts.rtc_engine, owner: opts.owner}
    {[], state}
  end

  @impl true
  def handle_parent_notification(message, _ctx, state) do
    send(state.owner, message)
    {[], state}
  end
end
