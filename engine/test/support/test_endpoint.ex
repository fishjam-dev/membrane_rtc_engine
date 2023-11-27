defmodule Membrane.RTC.Engine.Support.TestEndpoint do
  @moduledoc false

  use Membrane.Bin

  require Membrane.Logger

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              owner: [
                spec: pid(),
                default: nil,
                description: "Pid of owner of endpoint"
              ]

  def_input_pad :input,
    accepted_format: _any,
    availability: :on_request

  def_output_pad :output,
    accepted_format: _any,
    availability: :on_request

  @impl true
  def handle_init(_ctx, opts) do
    state = Map.from_struct(opts)

    {[], state}
  end

  @impl true
  def handle_parent_notification({:execute_actions, actions}, _ctx, state), do: {actions, state}

  @impl true
  def handle_parent_notification(_message, _ctx, %{owner: nil} = state) do
    {[], state}
  end

  @impl true
  def handle_parent_notification(message, _ctx, state) do
    send(state.owner, message)
    {[], state}
  end
end
