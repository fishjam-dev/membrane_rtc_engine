defmodule Membrane.RTC.Engine.Support.TestSink do
  @moduledoc false
  # more powerful version of membrane core Testing.Sink
  # it allows for executing arbitrary action using
  # :execute_actions message

  use Membrane.Sink

  alias Membrane.Testing.Notification

  def_input_pad :input,
    flow_control: :manual,
    demand_unit: :buffers,
    accepted_format: _any

  def_options autodemand: [
                type: :boolean,
                default: true,
                description: """
                If true element will automatically make demands.
                If it is set to false demand has to be triggered manually by sending `:make_demand` message.
                """
              ]

  @impl true
  def handle_init(_ctx, opts) do
    {[], opts}
  end

  @impl true
  def handle_playing(_context, %{autodemand: true} = state),
    do: {[demand: :input], state}

  def handle_playing(_context, state), do: {[], state}

  @impl true
  def handle_event(:input, event, _context, state) do
    {notify({:event, event}), state}
  end

  @impl true
  def handle_start_of_stream(pad, _ctx, state),
    do: {notify({:start_of_stream, pad}), state}

  @impl true
  def handle_end_of_stream(pad, _ctx, state),
    do: {notify({:end_of_stream, pad}), state}

  @impl true
  def handle_stream_format(pad, format, _context, state),
    do: {notify({:stream_format, pad, format}), state}

  @impl true
  def handle_parent_notification({:make_demand, size}, _ctx, %{autodemand: false} = state) do
    {[demand: {:input, size}], state}
  end

  @impl true
  def handle_parent_notification({:execute_actions, actions}, _ctx, state) do
    {actions, state}
  end

  @impl true
  def handle_buffer(:input, buf, _ctx, state) do
    case state do
      %{autodemand: false} -> {notify({:buffer, buf}), state}
      %{autodemand: true} -> {[demand: :input] ++ notify({:buffer, buf}), state}
    end
  end

  defp notify(payload) do
    [notify_parent: %Notification{payload: payload}]
  end
end
