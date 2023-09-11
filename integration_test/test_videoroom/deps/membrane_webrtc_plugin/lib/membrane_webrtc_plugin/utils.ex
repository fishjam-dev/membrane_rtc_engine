defmodule Membrane.WebRTC.Utils do
  @moduledoc false

  @doc """
  Converts encoding name to its SDP string representation.
  """
  @spec encoding_name_to_string(atom()) :: String.t()
  def encoding_name_to_string(encoding_name) do
    case encoding_name do
      :VP8 -> "VP8"
      :H264 -> "H264"
      :OPUS -> "opus"
      nil -> raise "Empty encoding name!"
      x -> to_string(x)
    end
  end

  @spec anonymize_sdp(String.t()) :: String.t()
  def anonymize_sdp(sdp) do
    Regex.replace(~r/(ice-ufrag|fingerprint|ice-pwd):.*\n/, sdp, "\\1:****\n")
  end
end
