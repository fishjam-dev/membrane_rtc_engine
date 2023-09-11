defmodule Membrane.ExLibSRTP.BundlexProject do
  use Bundlex.Project

  def project do
    [
      natives: natives()
    ]
  end

  defp natives() do
    [
      srtp: [
        interface: :nif,
        sources: [
          "srtp.c",
          "srtp_util.c",
          "unifex_util.c"
        ],
        pkg_configs: ["libsrtp2"],
        preprocessor: Unifex
      ]
    ]
  end
end
