if Code.ensure_loaded?(Membrane.RawAudio) do
  defmodule Membrane.RTC.Engine.Endpoint.HLS.MixerConfig do
    @moduledoc """
    Module representing mixer configuration for the HLS endpoint.
    """

    alias Membrane.RTC.Engine.Endpoint.HLS.{AudioMixerConfig, CompositorConfig}

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
