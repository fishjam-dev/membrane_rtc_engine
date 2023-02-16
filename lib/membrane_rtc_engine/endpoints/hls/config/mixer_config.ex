if Enum.all?([Membrane.RawAudio, Membrane.RawVideo], &Code.ensure_loaded?/1) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.MixerConfig do
    @moduledoc """
    Module representing mixer configuration for the HLS endpoint.
    """

    alias Membrane.RTC.Engine.Endpoint.HLS.{AudioMixerConfig, CompositorConfig}

    @typedoc """
    * `video` - video compositor configuration.
    * `audio` - audio mixer configuration.
    * `persist?` - if set to true, the stream will be continuous even if all tracks are removed, if then some new track is added it will belong to the same stream.
    Otherwise if set to false, stream will automatically end when the last track is removed, adding then new track will start a new hls stream.
    """
    @type t() :: %__MODULE__{
            video: CompositorConfig.t(),
            audio: AudioMixerConfig.t(),
            persist?: boolean()
          }
    defstruct video: %CompositorConfig{},
              audio: %AudioMixerConfig{},
              persist?: false
  end
end
