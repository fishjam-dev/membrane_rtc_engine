defmodule Membrane.H264.FFmpeg.BundlexProject do
  use Bundlex.Project

  def project() do
    [
      natives: natives()
    ]
  end

  defp natives() do
    [
      parser: [
        interface: :nif,
        sources: ["parser.c"],
        pkg_configs: ["libavcodec", "libavutil"],
        preprocessor: Unifex
      ],
      decoder: [
        interface: :nif,
        sources: ["decoder.c"],
        pkg_configs: ["libavcodec", "libavutil"],
        preprocessor: Unifex
      ],
      encoder: [
        interface: :nif,
        sources: ["encoder.c"],
        pkg_configs: ["libavcodec", "libavutil"],
        preprocessor: Unifex
      ]
    ]
  end
end
