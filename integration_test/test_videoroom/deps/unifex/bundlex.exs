defmodule Unifex.BundlexProject do
  use Bundlex.Project

  def project do
    [
      libs: libs()
    ]
  end

  defp libs do
    [
      unifex: [
        deps: [shmex: :shmex],
        src_base: "unifex/nif/unifex",
        sources: ["unifex.c", "payload.c"],
        interface: :nif
      ],
      unifex: [
        src_base: "unifex/cnode/unifex",
        sources: ["unifex.c", "cnode.c", "payload.c"],
        libs: ["pthread"],
        interface: :cnode
      ]
    ]
  end
end
