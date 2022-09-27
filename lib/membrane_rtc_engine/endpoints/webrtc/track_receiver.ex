defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver do
  @moduledoc false

  # TrackReceiver:
  # * generates probe packets on request from the
  # outside (todo)
  # * handles simulcast encoding selection
  # * adjusts RTP packets (sequence numbers, timestamps,
  # VP8 payload headers, etc.)

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Endpoint.WebRTC.{ConnectionProber, Forwarder, VariantSelector}

  alias Membrane.RTC.Engine.Track

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched
  }

  @typedoc """
  Message that can be sent to change target variant.

  Target variant is variant that will be forwarded
  whenever it is active.
  """
  @type set_target_variant_msg :: {:set_target_variant, Track.variant()}

  def_options track: [
                type: :struct,
                spec: Track.t(),
                description: "Track this adapter will maintain"
              ],
              initial_target_variant: [
                spec: Membrane.RTC.Engine.Track.variant(),
                description: """
                Track variant that will be forwarded whenever it is active.
                Can be changed with `t:set_target_variant_msg/0`.
                """
              ],
              connection_prober_pid: [
                spec: pid()
              ]

  def_input_pad :input,
    availability: :always,
    mode: :push,
    caps: Membrane.RTP

  def_output_pad :output,
    availability: :always,
    mode: :push,
    caps: Membrane.RTP

  @impl true
  def handle_init(%__MODULE__{
        connection_prober_pid: connection_prober_pid,
        track: track,
        initial_target_variant: initial_target_variant
      }) do
    forwarder = Forwarder.new(track.encoding, track.clock_rate)
    selector = VariantSelector.new(initial_target_variant)

    state = %{
      track: track,
      forwarder: forwarder,
      selector: selector,
      needs_reconfiguration: false,
      last_packet_metadata: nil,
      connection_prober: connection_prober_pid
    }

    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, state) do
    unless state.track.encoding == :OPUS,
      do: ConnectionProber.register_track_receiver(state.connection_prober, self())

    {:ok, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantSwitched{new_variant: new_variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    selector = VariantSelector.set_current_variant(state.selector, new_variant)
    actions = [notify: {:variant_switched, new_variant}]
    state = %{state | selector: selector, needs_reconfiguration: true}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantPaused{variant: variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_variant} = VariantSelector.variant_inactive(state.selector, variant)
    actions = maybe_request_track_variant(next_variant)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{variant: variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_variant} = VariantSelector.variant_active(state.selector, variant)
    actions = maybe_request_track_variant(next_variant)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    forwarder =
      if state.needs_reconfiguration,
        do: Forwarder.reconfigure(state.forwarder, buffer),
        else: state.forwarder

    {forwarder, buffer} = Forwarder.align(forwarder, buffer)
    actions = if buffer, do: [buffer: {:output, buffer}], else: []

    state = %{
      state
      | forwarder: forwarder,
        needs_reconfiguration: false,
        last_packet_metadata: buffer.metadata.rtp
    }

    ConnectionProber.buffer_sent(state.connection_prober, buffer)

    {{:ok, actions}, state}
  end

  @impl true
  def handle_other({:set_target_variant, variant}, _ctx, state) do
    if variant not in state.track.variants do
      raise("""
      Tried to set invalid target variant: #{inspect(variant)} for track: #{inspect(state.track)}.
      """)
    end

    {selector, next_variant} = VariantSelector.set_target_variant(state.selector, variant)
    actions = maybe_request_track_variant(next_variant)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(:send_padding_packet, ctx, state)
      when not is_nil(state.last_packet_metadata) and not ctx.pads.output.end_of_stream? do
    buffer = %Buffer{
      payload: <<>>,
      metadata: %{
        rtp: %{
          is_padding?: true,
          ssrc: state.last_packet_metadata.ssrc,
          extensions: [],
          csrcs: [],
          payload_type: state.last_packet_metadata.payload_type,
          marker: false,
          sequence_number: :unknown,
          timestamp: :unknown
        }
      }
    }

    {forwarder, buffer} = Forwarder.align(state.forwarder, buffer)

    {{:ok, buffer: {:output, buffer}}, %{state | forwarder: forwarder}}
  end

  @impl true
  def handle_other(:send_padding_packet, _ctx, state) do
    Process.send_after(self(), :send_padding_packet, 10)
    {:ok, state}
  end

  @impl true
  def handle_other(msg, ctx, state) do
    super(msg, ctx, state)
  end

  defp maybe_request_track_variant(nil), do: []

  defp maybe_request_track_variant(variant),
    do: [event: {:input, %RequestTrackVariant{variant: variant}}]
end
