defmodule Membrane.RTC.Engine.Support.TrackSender do
  @moduledoc false

  # This is a very simple version of WebRTC.TrackSender element.
  #
  # Engine forwards track starting from a keyframe.
  # Therefore, whenever some endpoint requests some
  # track, engine sends keyframe request to the track
  # source.
  #
  # In case of publishing from a file we are not able to
  # generate keyframe on request. This element ignores
  # incoming demands and starts sending data after receiving
  # first keyframe request. Subsequent keyframe requests
  # are ignored.

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Event.TrackVariantResumed
  alias Membrane.RTC.Engine.Track

  def_options track: [
                type: :struct,
                spec: Membrane.RTC.Engine.Track.t(),
                description: "Track this sender will maintain"
              ]

  def_input_pad :input,
    mode: :pull,
    demand_unit: :buffers,
    demand_mode: :manual,
    caps: Membrane.RTP

  def_output_pad :output,
    mode: :pull,
    demand_mode: :manual,
    caps: Membrane.RTP

  @impl true
  def handle_init(%__MODULE__{track: track}) do
    {:ok, %{track: track, started?: false}}
  end

  @impl true
  def handle_demand(:output, _size, :buffers, _ctx, %{started?: false} = state) do
    {:ok, state}
  end

  @impl true
  def handle_demand(:output, size, :buffers, _ctx, %{started?: true} = state) do
    {{:ok, demand: {:input, size}}, state}
  end

  @impl true
  def handle_prepared_to_playing(_ctx, state) do
    {{:ok, event: {:output, %TrackVariantResumed{variant: :high}}}, state}
  end

  @impl true
  def handle_event(:output, %Membrane.KeyframeRequestEvent{}, _ctx, state) do
    {{:ok, redemand: :output}, %{state | started?: true}}
  end

  @impl true
  def handle_process(:input, buffer, _ctx, state) do
    buffer = add_is_keyframe_flag(buffer, state.track)
    {{:ok, buffer: {:output, buffer}, redemand: :output}, state}
  end

  defp add_is_keyframe_flag(buffer, %Track{encoding: encoding}) do
    is_keyframe =
      case encoding do
        :OPUS -> true
        :H264 -> Membrane.RTP.H264.Utils.is_keyframe(buffer.payload)
        :VP8 -> Membrane.RTP.VP8.Utils.is_keyframe(buffer.payload)
      end

    new_metadata = Map.put(buffer.metadata, :is_keyframe, is_keyframe)
    %Buffer{buffer | metadata: new_metadata}
  end
end