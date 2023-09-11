defmodule Bunch.Native.BundlexProject do
  use Bundlex.Project

  def project do
    [
      libs: libs()
    ]
  end

  defp libs do
    [
      lib: [
        src_base: "bunch/bunch",
        sources: ["lib.c"]
      ],
      bunch: [
        interface: :nif,
        deps: [bunch_native: :lib],
        src_base: "bunch/nif/bunch",
        sources: ["bunch.c"]
      ]
    ]
  end
end
