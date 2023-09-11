defmodule Membrane.Common.C.BundlexProject do
  use Bundlex.Project

  def project do
    [
      libs: libs()
    ]
  end

  defp libs do
    [
      membrane: [
        interface: :nif,
        src_base: "membrane",
        sources: ["log.c"],
        preprocessor: Unifex
      ],
      membrane_ringbuffer: [
        interface: :nif,
        deps: [unifex: :unifex],
        src_base: "membrane_ringbuffer",
        sources: ["ringbuffer.c"]
      ]
    ]
  end
end
