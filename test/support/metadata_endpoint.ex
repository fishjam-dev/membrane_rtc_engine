defmodule MetadataEndpoint do
  @moduledoc false
  use Membrane.Bin

  alias Membrane.RTC.Engine
  require Membrane.Logger

  @type encoding_t() :: String.t()

  def_options rtc_engine: [
                spec: pid(),
                description: "Pid of parent Engine"
              ],
              track: [
                spec: Engine.Track.t(),
                description: "Test track"
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
    state = %{rtc_engine: opts.rtc_engine}

    {{:ok, notify: {:publish, {:new_tracks, [opts.track]}}}, state}
  end

  @impl true
  def handle_other({:new_tracks, []}, _ctx, state) do
    {:ok, state}
  end

  @impl true
  def handle_other({:custom_media_event, event}, _ctx, state) do
    Membrane.Logger.warn("Invalid media event #{inspect(event)}. Ignoring.")
    {:ok, state}
  end
end
