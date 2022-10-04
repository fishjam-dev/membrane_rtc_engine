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

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{Forwarder, VariantSelector}

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
                spec: Membrane.RTC.Engine.Track.t(),
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
  def handle_init(opts) do
    %__MODULE__{
      track: track,
      initial_target_variant: initial_target_variant,
      keyframe_request_interval: keyframe_request_interval
    } = opts

    forwarder = Forwarder.new(track.encoding, track.clock_rate)
    selector = VariantSelector.new(initial_target_variant)

    state = %{
      track: track,
      forwarder: forwarder,
      selector: selector,
      needs_reconfiguration: false,
      keyframe_request_interval: keyframe_request_interval
    }

    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, %{keyframe_request_interval: interval} = state) do
    actions = if interval, do: [start_timer: {:request_keyframe, interval}], else: []
    {{:ok, actions}, state}
  end

  @impl true
  def handle_tick(:request_keyframe, ctx, state) do
    {{:ok, maybe_request_keyframe(state.selector.current_variant, ctx)}, state}
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
  def handle_event(_pad, %TrackVariantPaused{variant: variant} = event, ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_variant} = VariantSelector.variant_inactive(state.selector, variant)
    actions = maybe_request_track_variant(next_variant, ctx)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{variant: variant} = event, ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_variant} = VariantSelector.variant_active(state.selector, variant)
    actions = maybe_request_track_variant(next_variant, ctx)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %Membrane.KeyframeRequestEvent{}, ctx, state) do
    {{:ok, maybe_request_keyframe(state.selector.current_variant, ctx)}, state}
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
    state = %{state | forwarder: forwarder, needs_reconfiguration: false}
    {{:ok, buffer: {:output, buffer}}, state}
  end

  @impl true
  def handle_other({:set_target_variant, variant}, ctx, state) do
    if variant not in state.track.variants do
      raise("""
      Tried to set invalid target variant: #{inspect(variant)} for track: #{inspect(state.track)}.
      """)
    end

    {selector, next_variant} = VariantSelector.set_target_variant(state.selector, variant)
    actions = maybe_request_track_variant(next_variant, ctx)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(msg, ctx, state) do
    super(msg, ctx, state)
  end

  # although `:input` pad is static it might not exist
  #
  # consider scenario when track receiver is encapsulated
  # into bin with dynamic input pad
  #
  # after unlinking bin's dynamic input pad
  # we are loosing our static pad
  defp maybe_request_keyframe(nil, _ctx), do: []

  defp maybe_request_keyframe(_current_variant, %{pads: %{input: _pad_data}}),
    do: [event: {:input, %Membrane.KeyframeRequestEvent{}}]

  defp maybe_request_track_variant(nil, _ctx), do: []

  defp maybe_request_track_variant(variant, %{pads: %{input: _pad_data}}),
    do: [event: {:input, %RequestTrackVariant{variant: variant}}]
end
