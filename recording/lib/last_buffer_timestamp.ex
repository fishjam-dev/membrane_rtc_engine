defmodule Membrane.RTC.Engine.Endpoint.Recording.LastBufferTimestamp do
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

  @impl true
  def handle_init(_ctx, options) do
    {[],
     %{
       first_buffer_timestamp: nil,
       counter: 0,
       reporter: options.reporter
     }}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, ctx, %{first_buffer_timestamp: nil} = state) do
    Recording.Reporter.start_track(state.reporter, track_id(ctx), buffer.metadata.rtp.timestamp)

    {[buffer: {:output, buffer}],
     %{state | first_buffer_timestamp: buffer.metadata.rtp.timestamp}}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, ctx, state) do
    counter =
      if state.counter + 1 == 200 do
        Recording.Reporter.end_track(state.reporter, track_id(ctx), buffer.metadata.rtp.timestamp)
        0
      else
        state.counter + 1
      end

    {[buffer: {:output, buffer}], %{state | counter: counter}}
  end

  defp track_id(%{name: {:last_buffer_timestamp, track_id}}), do: track_id
end
