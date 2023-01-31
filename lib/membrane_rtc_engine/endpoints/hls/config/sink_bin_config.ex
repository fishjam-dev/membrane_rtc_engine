defmodule Membrane.RTC.Engine.Endpoint.HLS.SinkBinConfig do
  @moduledoc """
  Module representing Membrane.HTTPAdaptiveStream.SinkBin configuration for the HLS endpoint.
  """

  @typedoc """
    To read more about config options go to module Membrane.HTTPAdaptiveStream.SinkBin and read options descriptions.
  """

  alias Membrane.HTTPAdaptiveStream.Manifest

  @type t() :: %__MODULE__{
          manifest_name: String.t(),
          manifest_module: (Path.t() -> module),
          storage: (Path.t() -> Storage.config_t()),
          target_window_duration: pos_integer | :infinity,
          persist?: boolean,
          mode: :live | :vod,
          hls_mode: :muxed_av | :separate_av,
          header_naming_fun: (Manifest.Track.t(), counter :: non_neg_integer() -> String.t()),
          segment_naming_fun: (Manifest.Track.t() -> String.t()),
          mp4_parameters_in_band?: boolean
        }

  defstruct manifest_name: "index",
            manifest_module: Membrane.HTTPAdaptiveStream.HLS,
            storage: &__MODULE__.default_storage/1,
            target_window_duration: Membrane.Time.seconds(40),
            persist?: false,
            mode: :live,
            hls_mode: :separate_av,
            header_naming_fun: &Manifest.Track.default_header_naming_fun/2,
            segment_naming_fun: &Manifest.Track.default_segment_naming_fun/1,
            mp4_parameters_in_band?: false

  @spec default_storage(String.t()) :: any
  def default_storage(directory),
    do: %Membrane.HTTPAdaptiveStream.Storages.FileStorage{directory: directory}
end
