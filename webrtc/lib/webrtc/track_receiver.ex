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

  TrackReceiver also emits some notifications. They are defined
  in `t:notifications/0`.
  """
  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{
    ConnectionAllocator.AllocationGrantedNotification,
    Forwarder,
    Metrics,
    VariantSelector
  }

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantBitrate,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched,
    VoiceActivityChanged
  }

  alias Membrane.RTC.Engine.Track
  alias Membrane.RTCP.SenderReportPacket
  alias Membrane.RTCPEvent

  @typedoc """
  Reason of track variant switch.

  * `:low_bandwidth` - bandwidth was too low to maintain current track quality
  * `:variant_inactive` - variant became inactive
  * `:set_bandwidth_allocation` - variant has been directly set
  * `:variant_active` - variant became active *AND* we have enough bandwidth to support it
  * `:automatic_selection` - variant became active but no target variant has been set so automatic selection has been performed
  * `:target_variant_selected` - variant has been selected as target and is active at the moment of selection
  * `:other` - it was hard to determine the exact reason
  """
  @type variant_switch_reason() :: :low_bandwidth | :variant_inactive | :other

  @typedoc """
  Messages that can be sent to TrackReceiver to control its behavior.
  """
  @type control_messages() :: set_target_variant() | set_negotiable?()

  @typedoc """
  Changes target track variant.

  Target track variant is variant that will be forwarded
  whenever it is active.
  """
  @type set_target_variant() :: {:set_target_variant, Track.variant()}

  @typedoc """
  Changes negotiability status of the TrackReceiver.

  Negotiability refers to the setting in `Membrane.RTC.Engine.Endpoint.WebRTC.ConnectionAllocator`
  that determines if the allocation for the TrackReceiver can be negotiated.
  """
  @type set_negotiable?() :: {:set_negotiable?, boolean()}

  @typedoc """
  Notifications that TrackReceiver emits.
  """
  @type notifications() :: variant_switched() | voice_activity_changed()

  @typedoc """
  Notification emitted whenever TrackReceiver starts receiving a new track variant.
  """
  @type variant_switched() :: {:variant_switched, Track.variant(), variant_switch_reason()}

  @typedoc """
  Notfication emitted when TrackReceiver receives an update on voice activity
  """
  @type voice_activity_changed() :: {:voice_activity_changed, :silence | :speech}

  @max_paddings 30

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
              ],
              allocation_negotiable?: [
                spec: boolean(),
                default: false,
                description: """
                Option defining whether allocation for this Track Receiver should be negotiable.

                Trying to enable negotiability for tracks that are inherently non-negotiable, also
                known as non-simulcast tracks, will result in a crash.

                This value can later be changed by sending a `set_negotiable?/0` control message to this Element.
                """
              ],
              telemetry_label: [
                spec: Membrane.TelemetryMetrics.label(),
                default: [],
                description: "Label passed to Membrane.TelemetryMetrics functions"
              ]

  def_input_pad :input,
    flow_control: :push,
    accepted_format: Membrane.RTP

  def_output_pad :output,
    flow_control: :push,
    accepted_format: Membrane.RTP

  @impl true
  def handle_init(_ctx, %__MODULE__{
        connection_allocator: connection_allocator,
        track: track,
        initial_target_variant: initial_target_variant,
        keyframe_request_interval: keyframe_request_interval,
        connection_allocator_module: connection_allocator_module,
        allocation_negotiable?: allocation_negotiable?,
        telemetry_label: telemetry_label
      }) do
    telemetry_label = telemetry_label ++ [track_id: "#{track.id}"]
    Metrics.register_variant_switched_event(telemetry_label)
    Metrics.register_paddings_sent_event(telemetry_label)

    forwarder = Forwarder.new(track.encoding, track.clock_rate)

    selector =
      VariantSelector.new(
        track,
        connection_allocator_module,
        connection_allocator,
        initial_target_variant: initial_target_variant,
        negotiable?: allocation_negotiable?
      )

    state = %{
      track: track,
      forwarder: forwarder,
      selector: selector,
      needs_reconfiguration: false,
      keyframe_request_interval: keyframe_request_interval,
      connection_allocator: connection_allocator,
      connection_allocator_module: connection_allocator_module,
      telemetry_label: telemetry_label,
      enqueued_paddings_count: 0
    }

    {[], state}
  end

  @impl true
  def handle_playing(_ctx, %{keyframe_request_interval: interval} = state) do
    actions = if interval, do: [start_timer: {:request_keyframe, interval}], else: []
    {actions, state}
  end

  @impl true
  def handle_tick(:request_keyframe, _ctx, state) do
    {maybe_request_keyframe(state.selector.current_variant), state}
  end

  @impl true
  def handle_event(_pad, %VoiceActivityChanged{voice_activity: vad}, _ctx, state) do
    {[notify_parent: {:voice_activity_changed, vad}], state}
  end

  @impl true
  def handle_event(
        _pad,
        %TrackVariantBitrate{variant: variant, bitrate: bitrate} = event,
        _ctx,
        state
      ) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")

    {[],
     %{
       state
       | selector: VariantSelector.update_variant_bitrate(state.selector, variant, bitrate)
     }}
  end

  @impl true
  def handle_event(_pad, %TrackVariantSwitched{} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")

    Metrics.emit_variant_switched_event(
      event.new_variant,
      event.reason,
      state.telemetry_label
    )

    selector = VariantSelector.set_current_variant(state.selector, event.new_variant)
    actions = [notify_parent: {:variant_switched, event.new_variant, event.reason}]
    state = %{state | selector: selector, needs_reconfiguration: true}
    {actions, state}
  end

  @impl true
  def handle_event(
        _pad,
        %TrackVariantPaused{variant: variant, reason: reason} = event,
        _ctx,
        state
      ) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")

    {selector, selector_action} = VariantSelector.variant_paused(state.selector, variant, reason)
    actions = handle_selector_action(selector_action)

    {actions, %{state | selector: selector}}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{variant: variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")

    {selector, selector_action} = VariantSelector.variant_resumed(state.selector, variant)
    actions = handle_selector_action(selector_action)

    {actions, %{state | selector: selector}}
  end

  @impl true
  def handle_event(_pad, %Membrane.KeyframeRequestEvent{}, _ctx, state) do
    {maybe_request_keyframe(state.selector.current_variant), state}
  end

  @impl true
  def handle_event(
        _pad,
        %RTCPEvent{rtcp: %SenderReportPacket{} = rtcp} = event,
        _ctx,
        state
      ) do
    {forwarder, rtcp} = Forwarder.align(state.forwarder, rtcp)
    {[event: {:output, Map.put(event, :rtcp, rtcp)}], %{state | forwarder: forwarder}}
  end

  @impl true
  def handle_event(pad, event, ctx, state) do
    super(pad, event, ctx, state)
  end

  @impl true
  def handle_buffer(_pad, buffer, _ctx, state) do
    state = update_bandwidth(buffer, :input, state)

    forwarder =
      if state.needs_reconfiguration,
        do: Forwarder.reconfigure(state.forwarder, buffer),
        else: state.forwarder

    {forwarder, buffer} = Forwarder.align(forwarder, buffer)
    state = %{state | forwarder: forwarder, needs_reconfiguration: false}

    {paddings, state} =
      if state.enqueued_paddings_count > 0 and Forwarder.can_generate_padding_packet?(forwarder) do
        if state.enqueued_paddings_count > @max_paddings do
          Membrane.Logger.warning(
            "Sending only #{@max_paddings} enqueued paddings out of #{state.enqueued_paddings_count}"
          )
        end

        paddings_num = min(state.enqueued_paddings_count, @max_paddings)
        paddings_bytes = paddings_num * 255

        Metrics.emit_paddings_sent_event(
          paddings_num,
          paddings_bytes,
          state.telemetry_label
        )

        Enum.map_reduce(
          # `//1` to prevent 1..0 from generating paddings
          1..paddings_num//1,
          %{state | enqueued_paddings_count: 0},
          fn _i, state -> generate_padding_packet(state, false) end
        )
      else
        {[], state}
      end

    actions =
      if buffer do
        state.connection_allocator_module.buffer_sent(state.connection_allocator, buffer)
        [buffer: {:output, [buffer | paddings]}]
      else
        []
      end

    {actions, state}
  end

  @impl true
  def handle_parent_notification({:set_negotiable, negotiable?}, _ctx, state) do
    VariantSelector.set_negotiable(state.selector, negotiable?)
    {[], state}
  end

  @impl true
  def handle_parent_notification({:set_target_variant, variant}, _ctx, state) do
    if variant not in state.track.variants do
      raise("""
      Tried to set invalid target variant: #{inspect(variant)} for track: #{inspect(state.track)}.
      """)
    end

    Membrane.Logger.debug("Setting target variant #{variant}")

    {selector, selector_action} = VariantSelector.set_target_variant(state.selector, variant)
    actions = handle_selector_action(selector_action)
    {actions, %{state | selector: selector}}
  end

  @impl true
  def handle_parent_notification({:bitrate_estimation, _estimation}, _ctx, state) do
    # Handle bitrate estimations of incoming variants
    # We're currently ignoring this information
    {[], state}
  end

  @impl true
  def handle_info(
        %AllocationGrantedNotification{allocation: allocation},
        _ctx,
        state
      ) do
    {selector, selector_action} =
      VariantSelector.set_bandwidth_allocation(state.selector, allocation)

    actions = handle_selector_action(selector_action)
    {actions, %{state | selector: selector}}
  end

  @impl true
  def handle_info(:decrease_your_allocation, _ctx, state) do
    {selector, selector_action} = VariantSelector.decrease_allocation(state.selector)
    actions = handle_selector_action(selector_action)
    {actions, %{state | selector: selector}}
  end

  # FIXME
  # this guard is too complicated and might mean
  # we are doing something incorrectly
  @impl true
  def handle_info(:send_padding_packet, ctx, state)
      when not ctx.pads.output.end_of_stream? and ctx.playback == :playing and
             ctx.pads.output.stream_format != nil do
    force_marker? =
      MapSet.size(state.selector.active_variants) == 0 or
        state.selector.current_variant == :no_variant

    if Forwarder.can_generate_padding_packet?(state.forwarder) or force_marker? do
      {buffer, state} = generate_padding_packet(state, force_marker?)
      {[buffer: {:output, buffer}], state}
    else
      {[], Map.update!(state, :enqueued_paddings_count, &(&1 + 1))}
    end
  end

  @impl true
  def handle_info(:send_padding_packet, _ctx, state) do
    {[], state}
  end

  @impl true
  def handle_info(msg, ctx, state) do
    super(msg, ctx, state)
  end

  defp maybe_request_keyframe(nil), do: []

  defp maybe_request_keyframe(_current_variant),
    do: [event: {:input, %Membrane.KeyframeRequestEvent{}}]

  defp handle_selector_action({:request, variant, reason}),
    do: [event: {:input, %RequestTrackVariant{variant: variant, reason: reason}}]

  defp handle_selector_action(_other_action), do: []

  defp generate_padding_packet(state, force_marker?) do
    {forwarder, buffer} =
      Forwarder.generate_padding_packet(state.forwarder, state.track, force_marker?)

    state = update_bandwidth(buffer, :padding, state)
    {buffer, %{state | forwarder: forwarder}}
  end

  defp update_bandwidth(buffer, id, state) do
    %{bandwidth: bandwidth, bandwidth_queue: queue} =
      Map.get(state, {:bandwidth, id}, %{bandwidth: 0, bandwidth_queue: Qex.new()})

    padding_size = Map.get(buffer.metadata.rtp, :padding_size, 0)
    buffer_size = bit_size(buffer.payload) + padding_size * 8
    buffer_time = Membrane.Time.monotonic_time()
    queue = Qex.push(queue, {buffer_size, buffer_time})
    {popped_size, first_buffer_time, queue} = pop_until(queue, buffer_time)

    bandwidth = %{
      bandwidth: bandwidth - popped_size + buffer_size,
      bandwidth_time: buffer_time - first_buffer_time,
      bandwidth_queue: queue
    }

    Map.put(state, {:bandwidth, id}, bandwidth)
  end

  defp pop_until(queue, time, size \\ 0) do
    {{buffer_size, buffer_time}, new_queue} = Qex.pop!(queue)

    if time - buffer_time < Membrane.Time.milliseconds(500) do
      {size, buffer_time, queue}
    else
      pop_until(new_queue, time, size + buffer_size)
    end
  end
end
