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
    VariantSelector
  }

  alias Membrane.RTC.Engine.Track

  alias Membrane.RTC.Engine.Track

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched,
    VoiceActivityChanged
  }

  alias Membrane.RTC.Engine.Track

  @typedoc """
  Reason of track variant switch.

  * `:low_bandwidth` - bandwidth was too low to maintain current track quality
  * `:variant_inactive` - variant became inactive
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
        connection_allocator_module: connection_allocator_module,
        allocation_negotiable?: allocation_negotiable?,
        telemetry_label: telemetry_label
      }) do
    telemetry_label = telemetry_label ++ [track_id: "#{track.id}"]
    Membrane.RTC.Utils.register_variant_switched_event(telemetry_label)

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
      telemetry_label: telemetry_label
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
  def handle_event(_pad, %VoiceActivityChanged{voice_activity: vad}, _ctx, state) do
    {{:ok, notify: {:voice_activity_changed, vad}}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantSwitched{} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")

    Membrane.RTC.Utils.emit_variant_switched_event(
      event.new_variant,
      event.reason,
      state.telemetry_label
    )

    selector = VariantSelector.set_current_variant(state.selector, event.new_variant)
    actions = [notify: {:variant_switched, event.new_variant, event.reason}]
    state = %{state | selector: selector, needs_reconfiguration: true}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantPaused{variant: variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, selector_action} = VariantSelector.variant_inactive(state.selector, variant)
    actions = handle_selector_action(selector_action)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{variant: variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, selector_action} = VariantSelector.variant_active(state.selector, variant)
    actions = handle_selector_action(selector_action)
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
    state = update_bandwidth(buffer, :input, state)

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
  def handle_other({:set_negotiable, negotiable?}, _ctx, state) do
    VariantSelector.set_negotiable(state.selector, negotiable?)
    {:ok, state}
  end

  @impl true
  def handle_other({:set_target_variant, variant}, _ctx, state) do
    if variant not in state.track.variants do
      raise("""
      Tried to set invalid target variant: #{inspect(variant)} for track: #{inspect(state.track)}.
      """)
    end

    Membrane.Logger.debug("Setting target variant #{variant}")

    {selector, selector_action} = VariantSelector.set_target_variant(state.selector, variant)
    actions = handle_selector_action(selector_action)
    {{:ok, actions}, %{state | selector: selector}}
  end

  # FIXME
  # this guard is too compilcated and might mean
  # we are doing something incorrectly
  @impl true
  def handle_other(:send_padding_packet, ctx, state)
      when not ctx.pads.output.end_of_stream? and ctx.playback_state == :playing and
             ctx.pads.output.caps != nil do
    {forwarder, buffer} = Forwarder.generate_padding_packet(state.forwarder, state.track)

    {actions, state} =
      if buffer do
        state.connection_allocator_module.probe_sent(state.connection_allocator)
        {[buffer: {:output, buffer}], update_bandwidth(buffer, :padding, state)}
      else
        Membrane.Logger.warn("Padding miss")
        {[], state}
      end

    {{:ok, actions}, %{state | forwarder: forwarder}}
  end

  @impl true
  def handle_other(:send_padding_packet, _ctx, state) do
    # Process.send_after(self(), :send_padding_packet, 100)
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
    {selector, selector_action} =
      VariantSelector.set_bandwidth_allocation(state.selector, allocation)

    actions = handle_selector_action(selector_action)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(:decrease_your_allocation, _ctx, state) do
    {selector, selector_action} = VariantSelector.decrease_allocation(state.selector)
    actions = handle_selector_action(selector_action)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(msg, ctx, state) do
    super(msg, ctx, state)
  end

  defp maybe_request_keyframe(nil), do: []

  defp maybe_request_keyframe(_current_variant),
    do: [event: {:input, %Membrane.KeyframeRequestEvent{}}]

  defp handle_selector_action({:request, variant, reason}),
    do: [event: {:input, %RequestTrackVariant{variant: variant, reason: reason}}]

  defp handle_selector_action(_other_action), do: []

  defp update_bandwidth(buffer, id, state) do
    require Membrane.Core.Metrics

    %{bandwidth: bandwidth, bandwidth_queue: queue} =
      Map.get(state, {:bandwidth, id}, %{bandwidth: 0, bandwidth_queue: Qex.new()})

    buffer_size = bit_size(buffer.payload) + buffer.metadata.rtp.padding_size * 8
    buffer_time = Membrane.Time.monotonic_time()
    queue = Qex.push(queue, {buffer_size, buffer_time})
    {popped_size, first_buffer_time, queue} = pop_until(queue, buffer_time)

    %{
      bandwidth: bandwidth - popped_size + buffer_size,
      bandwidth_time: buffer_time - first_buffer_time,
      bandwidth_queue: queue
    }
    |> tap(
      &Membrane.Core.Metrics.report(
        :bandwidth,
        &1.bandwidth / 1024 * 2,
        id: id
      )
    )
    |> then(&Map.put(state, {:bandwidth, id}, &1))
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
