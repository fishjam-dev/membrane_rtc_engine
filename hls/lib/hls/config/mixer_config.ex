if Enum.all?([Membrane.RawAudio, Membrane.RawVideo], &Code.ensure_loaded?/1) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.MixerConfig do
    @moduledoc """
    Module representing mixer configuration for the HLS endpoint.
    """

    alias Membrane.RTC.Engine.Endpoint.HLS.{AudioMixerConfig, CompositorConfig}

    @typedoc """
    * `video` - video compositor configuration.
    * `audio` - audio mixer configuration.
    """
    @type t() :: %__MODULE__{
            video: CompositorConfig.t(),
            audio: AudioMixerConfig.t()
          }
    defstruct video: %CompositorConfig{},
              audio: %AudioMixerConfig{}
  end
end
