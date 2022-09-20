defmodule Membrane.KeyframeMetadataAdder do
  use Membrane.Filter

  alias Membrane.RTC.Engine.Track
  alias Membrane.Buffer

  def_input_pad :input, demand_mode: :auto, caps: :any
  def_output_pad :output, demand_mode: :auto, caps: :any

  def_options track: [spec: Track.t()]

  @impl true
  def handle_init(opts), do: {:ok, %{track: opts.track}}

  def handle_process(_pad, buffer, _ctx, state) do
    buffer = add_is_keyframe_flag(buffer, state.track)

    {{:ok, forward: buffer}, state}
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
