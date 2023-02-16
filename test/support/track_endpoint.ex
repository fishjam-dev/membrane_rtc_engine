defmodule Membrane.RTC.Engine.Support.TrackEndpoint do
  @moduledoc false

  # Simple endpoint that publishes track passed in options

  use Membrane.Bin

  alias Membrane.RTC.Engine

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              track: [
                spec: Engine.Track.t(),
                description: "Track to publish"
              ],
              peer_metadata: [
                spec: any(),
                default: %{},
                description: """
                If the endpoint is registered as peer endpoint, the value of this field
                will be set as its metadata.
                """
              ]

  def_input_pad :input,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  def_output_pad :output,
    demand_unit: :buffers,
    caps: :any,
    availability: :on_request

  @impl true
  def handle_init(opts) do
    state = Map.from_struct(opts)

    {:ok, state}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, state) do
    {{:ok,
      notify: {:ready, state.peer_metadata}, notify: {:publish, {:new_tracks, [state.track]}}},
     state}
  end

  @impl true
  def handle_other({:execute_actions, actions}, _ctx, state), do: {{:ok, actions}, state}

  @impl true
  def handle_other({:new_tracks, []}, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_other(_message, _ctx, state), do: {:ok, state}
end
