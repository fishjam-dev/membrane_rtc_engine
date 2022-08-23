defmodule Membrane.RTC.Engine.Support.TestSink do
  @moduledoc false
  # more powerful version of membrane core Testing.Sink
  # it allows for executing arbitrary action using
  # :execute_actions message

  use Membrane.Sink

  alias Membrane.Testing.Notification

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any

  def_options autodemand: [
                type: :boolean,
                default: true,
                description: """
                If true element will automatically make demands.
                If it is set to false demand has to be triggered manually by sending `:make_demand` message.
                """
              ]

  @impl true
  def handle_init(opts) do
    {:ok, opts}
  end

  @impl true
  def handle_prepared_to_playing(_context, %{autodemand: true} = state),
    do: {{:ok, demand: :input}, state}

  def handle_prepared_to_playing(_context, state), do: {:ok, state}

  @impl true
  def handle_event(:input, event, _context, state) do
    {{:ok, notify({:event, event})}, state}
  end

  @impl true
  def handle_start_of_stream(pad, _ctx, state),
    do: {{:ok, notify({:start_of_stream, pad})}, state}

  @impl true
  def handle_end_of_stream(pad, _ctx, state),
    do: {{:ok, notify({:end_of_stream, pad})}, state}

  @impl true
  def handle_caps(pad, caps, _context, state),
    do: {{:ok, notify({:caps, pad, caps})}, state}

  @impl true
  def handle_other({:make_demand, size}, _ctx, %{autodemand: false} = state) do
    {{:ok, demand: {:input, size}}, state}
  end

  @impl true
  def handle_other({:execute_actions, actions}, _ctx, state) do
    {{:ok, actions}, state}
  end

  @impl true
  def handle_write(:input, buf, _ctx, state) do
    case state do
      %{autodemand: false} -> {{:ok, notify({:buffer, buf})}, state}
      %{autodemand: true} -> {{:ok, [demand: :input] ++ notify({:buffer, buf})}, state}
    end
  end

  defp notify(payload) do
    [notify: %Notification{payload: payload}]
  end
end
