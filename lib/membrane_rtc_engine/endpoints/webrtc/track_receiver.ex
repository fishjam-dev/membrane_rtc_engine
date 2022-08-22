defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackReceiver do
  @moduledoc false

  # TrackReceiver:
  # * generates probe packets on request from the
  # outside
  # * switches between simulcast layers on request from
  # the outside
  # * adjusts RTP packets (sequence numbers, timestamps,
  # VP8 payload headers, etc.)

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.RTC.Engine.Event.{TrackVariantPaused, TrackVariantResumed, TrackVariantSwitched}

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this adapter will maintain"
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
  def handle_init(%__MODULE__{track: track}) do
    state = %{track: track}
    {:ok, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantSwitched{} = event, _context, state) do
    Membrane.Logger.info("Got event: #{inspect(event)}")
    {:ok, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantPaused{} = event, _cotnext, state) do
    Membrane.Logger.info("Got event: #{inspect(event)}")
    {:ok, state}
  end

  @impl true
  def handle_event(_pad, %TrackVariantResumed{} = event, _context, state) do
    Membrane.Logger.info("Got event: #{inspect(event)}")
    {:ok, state}
  end

  @impl true
  def handle_event(pad, event, context, state) do
    super(pad, event, context, state)
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    actions = [buffer: {:output, buffer}]
    {{:ok, actions}, state}
  end
end
