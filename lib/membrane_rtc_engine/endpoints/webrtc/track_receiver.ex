defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver do
  @moduledoc """
  Element responsible for handling WebRTC track.

  Its main responsibility is to request the highest available
  track variant. If currently used track variant becomes inactive,
  TrackReceiver will switch to the next available variant.

  Outgoing RTP packets belong to the same sequence number
  and timestamp spaces but they are not guaranteed to be in order and
  contiguous.

  To unpack RTP see `Membrane.RTC.Engine.Track.get_depayloader/1`.

  To control TrackReceiver behavior see `t:control_messages/0`.

  TrackReceiver also emits some notificaitons. They are defined
  in `t:notifications/0`.
  """
  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{
    ConnectionAllocator.AllocationGrantedNotification,
    Forwarder,
    VariantSelector
  }

  alias Membrane.RTC.Engine.Track

  alias Membrane.RTC.Engine.Track

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched
  }

  alias Membrane.RTC.Engine.Track

  @typedoc """
  Messages that can be sent to TrackReceiver to control its behavior.
  """
  @type control_messages() :: set_target_variant()

  @typedoc """
  Changes target track variant.

  Target track variant is variant that will be forwarded
  whenever it is active.
  """
  @type set_target_variant() :: {:set_target_variant, Track.variant()}

  @typedoc """
  Notifications that TrackReceiver emits.
  """
  @type notifications() :: variant_switched()

  @typedoc """
  Notification emitted whenever TrackReceiver starts receiving a new track variant.
  """
  @type variant_switched() :: {:variant_switched, Track.variant()}

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
              keyframe_request_interval: [
                spec: Membrane.Time.t() | nil,
                default: nil,
                description: """
                Defines how often keyframe requests should be sent for
                currently used track variant.

                This option should be used in very specific cases (e.g. see HLS endpoint)
                as generating keyframes increases track bitrate and might introduce
                additional delay.
                """
              ],
              connection_allocator_module: [
                spec: module(),
                default: Membrane.RTC.Engine.Endpoint.WebRTC.NoOpConnectionAllocator,
                description: """
                Module implementing `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator` behavior
                that should be used by the TrackReceiver.
                """
              ],
              connection_allocator: [
                spec: pid() | nil,
                default: nil,
                description: """
                PID of the instance of the ConnectionAllocator that should be used by the TrackReceiver
                """
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
        connection_allocator: connection_allocator,
        track: track,
        initial_target_variant: initial_target_variant,
        keyframe_request_interval: keyframe_request_interval,
        connection_allocator_module: connection_allocator_module
      }) do
    forwarder = Forwarder.new(track.encoding, track.clock_rate)

    selector =
      VariantSelector.new(
        track,
        connection_allocator_module,
        connection_allocator,
        initial_target_variant
      )

    state = %{
      track: track,
      forwarder: forwarder,
      selector: selector,
      needs_reconfiguration: false,
      keyframe_request_interval: keyframe_request_interval,
      connection_allocator: connection_allocator,
      connection_allocator_module: connection_allocator_module
    }

    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, %{keyframe_request_interval: interval} = state) do
    actions = if interval, do: [start_timer: {:request_keyframe, interval}], else: []
    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:request_keyframe, _ctx, state) do
    {{:ok, maybe_request_keyframe(state.selector.current_variant)}, state}
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
  def handle_event(_pad, %Membrane.KeyframeRequestEvent{}, _ctx, state) do
    {{:ok, maybe_request_keyframe(state.selector.current_variant)}, state}
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

    state = %{
      state
      | forwarder: forwarder,
        needs_reconfiguration: false
    }

    actions =
      if buffer do
        state.connection_allocator_module.buffer_sent(state.connection_allocator, buffer)
        [buffer: {:output, buffer}]
      else
        []
      end

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
      when not ctx.pads.output.end_of_stream? do
    {forwarder, buffer} = Forwarder.generate_padding_packet(state.forwarder, state.track)

    actions =
      if buffer do
        state.connection_allocator_module.probe_sent(state.connection_allocator)
        [buffer: {:output, buffer}]
      else
        []
      end

    {{:ok, actions}, %{state | forwarder: forwarder}}
  end

  @impl true
  def handle_other(:send_padding_packet, _ctx, state) do
    Process.send_after(self(), :send_padding_packet, 10)
    {:ok, state}
  end

  @impl true
  def handle_other({:bitrate_estimation, _estimation}, _ctx, state) do
    # Handle bitrate estimations of incoming variants
    # We're currently ignoring this information
    {:ok, state}
  end

  @impl true
  def handle_other(
        %AllocationGrantedNotification{allocation: allocation},
        _ctx,
        state
      ) do
    {selector, variant} = VariantSelector.set_bandwidth_allocation(state.selector, allocation)
    actions = maybe_request_track_variant(variant)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(msg, ctx, state) do
    super(msg, ctx, state)
  end

  defp maybe_request_keyframe(nil), do: []

  defp maybe_request_keyframe(_current_variant),
    do: [event: {:input, %Membrane.KeyframeRequestEvent{}}]

  defp maybe_request_track_variant(nil), do: []

  defp maybe_request_track_variant(variant),
    do: [event: {:input, %RequestTrackVariant{variant: variant}}]
end
