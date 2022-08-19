defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender do
  @moduledoc false

  # TrackSender:
  # * adds `is_keyframe` flag to each buffer's metadata
  # (will be removed after releasing new RTP plugin)
  # * tracks encoding activity

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.WebRTC.TrackSenderState

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this sender will maintain"
              ]

  def_input_pad :input,
    availability: :always,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  def_output_pad :output,
    availability: :always,
    mode: :pull,
    demand_mode: :auto,
    caps: Membrane.RTP

  @impl true
  def handle_init(%__MODULE__{track: track}) do
    state = TrackSenderState.new(track)
    {:ok, state}
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, state) do
    {state, actions} = TrackSenderState.process(state, buffer)
    {{:ok, actions}, state}
  end
end
