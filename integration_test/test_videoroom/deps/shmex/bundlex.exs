defmodule Shmex.BundlexProject do
  use Bundlex.Project

  def project do
    [
      natives: natives(),
      libs: libs()
    ]
  end

  defp natives() do
    [
      shmex: [
        interface: :nif,
        deps: [shmex: :shmex, bunch_native: :bunch],
        sources: ["shmex.c"]
      ]
    ]
  end

  defp libs() do
    [
      lib: [
        src_base: "shmex/shmex",
        sources: ["lib.c"],
        libs: if(Bundlex.platform() == :linux, do: ["rt"], else: [])
      ],
      shmex: [
        interface: :nif,
        deps: [shmex: :lib, bunch_native: :bunch],
        src_base: "shmex/nif/shmex",
        sources: ["shmex.c"]
      ],
      shmex: [
        interface: :cnode,
        deps: [shmex: :lib],
        src_base: "shmex/cnode/shmex",
        sources: ["shmex.c"]
      ]
    ]
  end
end
