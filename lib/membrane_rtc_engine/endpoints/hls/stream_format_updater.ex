defmodule Membrane.RTC.Engine.Endpoint.HLS.StreamFormatUpdater do
  @moduledoc """
    Element responsible for sending stream format for specific track to HLS Endpoint and waiting for Endpoint to send updated layout to compositor element.
  """

  use Membrane.Filter

  def_input_pad :input,
    accepted_format: _any,
    demand_unit: :buffers,
    demand_mode: :auto

  def_output_pad :output,
    accepted_format: _any,
    demand_mode: :auto

  @impl true
  def handle_init(_ctx, _opts) do
    {[], %{update_queue: 0, buffers: %{}, end_of_stream: false}}
  end

  @impl true
  def handle_stream_format(_pad, stream_format, _ctx, state) do
    state = put_in(state, [:buffers, state.update_queue + 1], [])

    {[forward: stream_format, notify_parent: {:update_layout, stream_format}],
     %{state | update_queue: state.update_queue + 1}}
  end

  @impl true
  def handle_end_of_stream(_pad, _ctx, %{update__queue: 0} = state),
    do: {[end_of_stream: :output], state}

  @impl true
  def handle_end_of_stream(_pad, _ctx, state), do: {[], %{state | end_of_stream: true}}

  @impl true
  def handle_process(_pad, buffer, _ctx, %{update_queue: 0} = state),
    do: {[buffer: {:output, buffer}], state}

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    state = update_in(state, [:buffers, state.update_queue], &[buffer | &1])
    {[], state}
  end

  @impl true
  def handle_parent_notification(:layout_updated, _ctx, state) do
    {buffers, state} = pop_in(state, [:buffers, state.update_queue])
    buffers = Enum.reverse(buffers)
    actions = [buffer: {:output, buffers}] ++ maybe_notify_end_of_stream(state)
    {actions, %{state | update_queue: state.update_queue - 1}}
  end

  defp maybe_notify_end_of_stream(%{end_of_stream: true, update_queue: 0}),
    do: [end_of_stream: :output]

  defp maybe_notify_end_of_stream(_state), do: []
end
