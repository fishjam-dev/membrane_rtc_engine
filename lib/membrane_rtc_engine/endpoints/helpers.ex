defmodule WebRTCToHLS.Helpers do
  @moduledoc false

  @spec hls_output_path(prefix :: String.t()) :: String.t()
  def hls_output_path(prefix) do
    [hls_output_mount_path(), prefix] |> Path.join()
  end

  @spec hls_output_path(prefix :: String.t(), filename :: String.t()) :: String.t()
  def hls_output_path(prefix, filename) do
    [hls_output_mount_path(), prefix, filename] |> Path.join()
  end

  @spec hls_output_mount_path() :: String.t()
  def hls_output_mount_path(),
    do: Application.get_env(:membrane_webrtc_to_hls_demo, :hls_output_mount_path, "./hls_output")
end
