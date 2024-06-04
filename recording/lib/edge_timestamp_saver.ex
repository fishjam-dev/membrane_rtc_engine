defmodule Membrane.RTC.Engine.Endpoint.Recording.EdgeTimestampSaver do
  @moduledoc false

  use Membrane.Filter

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.Recording
  alias Membrane.RTCP.SenderReportPacket
  alias Membrane.RTCPEvent

  def_input_pad :input, accepted_format: _accepted_format

  def_output_pad :output, accepted_format: _accepted_format

  def_options reporter: [
                spec: pid(),
                description: """
                Pid of the recording reporter
                """
              ]

  @packets_interval 100
  @breakpoint 2 ** 31

  defguardp in_order?(delta) when delta < -@breakpoint or (delta > 0 and delta < @breakpoint)

  @impl true
  def handle_init(_ctx, options) do
    {[],
     %{
       first_buffer_timestamp: nil,
       last_buffer_timestamp: nil,
       counter: 0,
       reporter: options.reporter,
       rtcp_sent?: false
     }}
  end

  @impl true
  def handle_event(
        _pad,
        %RTCPEvent{rtcp: %SenderReportPacket{} = rtcp} = event,
        context,
        %{rtcp_sent?: false} = state
      )
      when not is_nil(state.first_buffer_timestamp) do
    Recording.Reporter.rtcp_packet(state.reporter, track_id(context), rtcp)
    {[event: {:output, event}], %{state | rtcp_sent?: true}}
  end

  @impl true
  def handle_event(pad, event, context, state) do
    super(pad, event, context, state)
  end

  @impl true
  def handle_buffer(
        _pad,
        %Buffer{} = buffer,
        ctx,
        %{first_buffer_timestamp: nil} = state
      ) do
    timestamp = buffer.metadata.rtp.timestamp

    Recording.Reporter.start_timestamp(
      state.reporter,
      track_id(ctx),
      timestamp
    )

    {[buffer: {:output, buffer}],
     %{state | first_buffer_timestamp: timestamp, last_buffer_timestamp: timestamp}}
  end

  @impl true
  def handle_buffer(
        _pad,
        %Buffer{} = buffer,
        ctx,
        %{first_buffer_timestamp: first_buffer_timestamp} = state
      )
      when in_order?(first_buffer_timestamp - buffer.metadata.rtp.timestamp) do
    timestamp = buffer.metadata.rtp.timestamp

    Recording.Reporter.start_timestamp(
      state.reporter,
      track_id(ctx),
      timestamp
    )

    {[buffer: {:output, buffer}], %{state | first_buffer_timestamp: timestamp}}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, _ctx, state)
      when not in_order?(buffer.metadata.rtp.timestamp - state.last_buffer_timestamp) do
    counter = min(state.counter + 1, @packets_interval - 1)
    {[buffer: {:output, buffer}], %{state | counter: counter}}
  end

  @impl true
  def handle_buffer(_pad, %Buffer{} = buffer, ctx, state) do
    timestamp = buffer.metadata.rtp.timestamp
    counter = rem(state.counter + 1, @packets_interval)

    if counter == 0,
      do:
        Recording.Reporter.end_timestamp(
          state.reporter,
          track_id(ctx),
          timestamp
        )

    state = %{state | counter: counter, last_buffer_timestamp: timestamp}

    {[buffer: {:output, buffer}], state}
  end

  @impl true
  def handle_end_of_stream(_pad, _ctx, %{last_buffer_timestamp: nil} = state) do
    {[end_of_stream: :output], state}
  end

  @impl true
  def handle_end_of_stream(_pad, ctx, state) do
    Recording.Reporter.end_timestamp(state.reporter, track_id(ctx), state.last_buffer_timestamp)
    {[end_of_stream: :output], state}
  end

  defp track_id(%{name: {:edge_timestamp_saver, track_id}}), do: track_id
end
