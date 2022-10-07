defmodule Membrane.RTC.Engine.Endpoint.HLS.MetadataFixer do
  use Membrane.Filter

  alias Membrane.Buffer

  def_input_pad :input, demand_mode: :auto, caps: [Membrane.H264, Membrane.Opus, Membrane.AAC]
  def_output_pad :output, demand_mode: :auto, caps: [Membrane.H264, Membrane.Opus, Membrane.AAC]

  @impl true
  def handle_init(_opts), do: {:ok, %{}}

  @impl true
  def handle_process(Pad.ref(:input), buffer, _ctx, state) do
    # buffer may not have metadata.h264 field, thus get_in is used
    is_keyframe = case get_in(Map.from_struct(buffer), [:metadata, :h264, :key_frame?]) do
      true -> true
      nil -> true
      _ -> false
    end
    buffer = %Buffer{buffer | metadata: %{is_keyframe: is_keyframe}}
    {{:ok, forward: buffer}, state}
  end
end
