defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSender do
  @moduledoc false

  # TrackSender:
  # * adds `is_keyframe` flag to each buffer's metadata
  # (will be removed after releasing new RTP plugin)
  # * tracks encoding activity

  use Membrane.Filter

  require Membrane.Logger

  alias Membrane.Buffer
  alias Membrane.RTC.Engine.Track

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
    {:ok, %{track: track}}
  end

  @impl true
  def handle_process(_pad, buffer, _ctx, %{track: track} = state) do
    buffer = add_is_keyframe_flag(buffer, track)
    {{:ok, [buffer: {:output, buffer}]}, state}
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
