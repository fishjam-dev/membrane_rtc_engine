defmodule Membrane.RTC.Engine.Endpoint.WebRTC.TrackSenderState do
  @moduledoc false

  alias Membrane.Buffer
  alias Membrane.Element.Action
  alias Membrane.RTC.Engine.Track

  @type t() :: %__MODULE__{track: Track.t()}

  @enforce_keys [:track]
  defstruct @enforce_keys

  @spec new(Track.t()) :: t()
  def new(track) do
    %__MODULE__{track: track}
  end

  @spec process(t(), Buffer.t()) :: {t(), [Action.t()]}
  def process(state, buffer) do
    buffer = add_is_keyframe_flag(state.track, buffer)
    actions = [buffer: {:output, buffer}]
    {state, actions}
  end

  defp add_is_keyframe_flag(%Track{encoding: encoding}, buffer) do
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
