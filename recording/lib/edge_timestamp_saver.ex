defmodule Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaver do
  @moduledoc false

  use Membrane.Filter

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.Recording

  def_input_pad :input, accepted_format: _accepted_format

  def_output_pad :output, accepted_format: _accepted_format

  def_options reporter: [
                spec: pid(),
                description: """
                Pid of the recording reporter
                """
              ]

  @packets_interval 200

  @breakpoint 2 ** 31

  @impl true
  def handle_init(_ctx, options) do
    {[],
     %{
       first_buffer_timestamp: nil,
       last_buffer_timestamp: nil,
       counter: 0,
       reporter: options.reporter
     }}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, ctx, %{first_buffer_timestamp: nil} = state) do
    Recording.Reporter.start_timestamp(
      state.reporter,
      track_id(ctx),
      buffer.metadata.rtp.timestamp
    )

    {[buffer: {:output, buffer}],
     %{state | first_buffer_timestamp: buffer.metadata.rtp.timestamp}}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, ctx, state) do
    counter = rem(state.counter + 1, @packets_interval)

    if counter == 0,
      do:
        Recording.Reporter.end_timestamp(
          state.reporter,
          track_id(ctx),
          buffer.metadata.rtp.timestamp
        )

    # a packet is in order when it is from the next cycle, or from current cycle with delta > 0
    delta =
      buffer.metadata.rtp.timestamp -
        (state.last_buffer_timestamp || state.first_buffer_timestamp)

    in_order? = delta < -@breakpoint or (delta > 0 and delta < @breakpoint)

    state =
      if in_order? do
        %{state | counter: counter, last_buffer_timestamp: buffer.metadata.rtp.timestamp}
      else
        %{state | counter: counter}
      end

    {[buffer: {:output, buffer}], state}
  end

  @impl true
  def handle_end_of_stream(_pad, ctx, state) do
    Recording.Reporter.end_timestamp(state.reporter, track_id(ctx), state.last_buffer_timestamp)
    {[end_of_stream: :output], state}
  end

  defp track_id(%{name: {:edge_timestamp_saver, track_id}}), do: track_id
end
