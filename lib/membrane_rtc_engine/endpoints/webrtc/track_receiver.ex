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

  alias Membrane.RTC.Engine.Endpoint.WebRTC.{EncodingSelector, Forwarder}

  alias Membrane.RTC.Engine.Event.{
    RequestTrackVariant,
    TrackVariantPaused,
    TrackVariantResumed,
    TrackVariantSwitched
  }

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this adapter will maintain"
              ],
              default_simulcast_encoding: [
                spec: Membrane.RTC.Engine.Track.variant(),
                description: "Simulcast encoding that will be forwarded by default."
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
        track: track,
        default_simulcast_encoding: default_simulcast_encoding
      }) do
    forwarder = Forwarder.new(track.encoding, track.clock_rate)
    selector = EncodingSelector.new(track.simulcast_encodings, default_simulcast_encoding)

    state = %{
      track: track,
      forwarder: forwarder,
      selector: selector,
      needs_reconfiguration: false
    }

    {:ok, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantSwitched{new_variant: new_variant} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    selector = EncodingSelector.set_current_encoding(state.selector, new_variant)
    actions = [notify: {:encoding_switched, new_variant}]
    state = %{state | selector: selector, needs_reconfiguration: true}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantPaused{variant: encoding} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_encoding} = EncodingSelector.encoding_inactive(state.selector, encoding)
    actions = maybe_request_track_encoding(next_encoding)
    state = %{state | selector: selector}
    {{:ok, actions}, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{variant: encoding} = event, _ctx, state) do
    Membrane.Logger.debug("Received event: #{inspect(event)}")
    {selector, next_encoding} = EncodingSelector.encoding_active(state.selector, encoding)
    actions = maybe_request_track_encoding(next_encoding)
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
    state = %{state | forwarder: forwarder, needs_reconfiguration: false}
    {{:ok, buffer: {:output, buffer}}, state}
  end

  @impl true
  def handle_other({:select_encoding, encoding}, _ctx, state) do
    if encoding not in state.selector.all_encodings,
      do: raise("Requested encoding #{encoding} isn't valid")

    {selector, next_encoding} = EncodingSelector.set_target_encoding(state.selector, encoding)
    actions = maybe_request_track_encoding(next_encoding)
    {{:ok, actions}, %{state | selector: selector}}
  end

  @impl true
  def handle_other(msg, ctx, state) do
    super(msg, ctx, state)
  end

  defp maybe_request_track_encoding(nil), do: []

  defp maybe_request_track_encoding(encoding),
    do: [event: {:input, %RequestTrackVariant{variant: encoding}}]
end
