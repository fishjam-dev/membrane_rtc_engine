defmodule ExSDP.Serializer do
  @moduledoc """
  Module providing helper functions for serialization.
  """

  @doc """
  Serializes both sdp lines (<type>=<value>) and sdp parameters (<parameter>=<value>)
  """
  @spec maybe_serialize(type :: binary(), value :: term()) :: binary()
  def maybe_serialize(_type, nil), do: ""
  def maybe_serialize(_type, []), do: ""

  def maybe_serialize(type, values) when is_list(values),
    do: Enum.map_join(values, "\n", fn value -> maybe_serialize(type, value) end)

  def maybe_serialize(type, {:framerate, {frames, sec}}),
    do: "#{type}=framerate:#{frames}/#{sec}"

  def maybe_serialize(type, {:ice_ufrag, value}), do: "#{type}=ice-ufrag:#{value}"
  def maybe_serialize(type, {:ice_pwd, value}), do: "#{type}=ice-pwd:#{value}"

  def maybe_serialize(type, :ice_lite), do: "#{type}=ice-lite"

  def maybe_serialize(type, {:ice_options, value}),
    do: "#{type}=ice-options:#{serialize_ice_options(value)}"

  def maybe_serialize(type, {:fingerprint, value}),
    do: "#{type}=fingerprint:#{serialize_fingerprint(value)}"

  def maybe_serialize(type, {:setup, value}), do: "#{type}=setup:#{serialize_setup(value)}"
  def maybe_serialize(type, {:mid, value}), do: "#{type}=mid:#{value}"
  def maybe_serialize(type, :rtcp_mux), do: "#{type}=rtcp-mux"
  def maybe_serialize(type, :rtcp_rsize), do: "#{type}=rtcp-rsize"

  def maybe_serialize(type, true), do: "#{type}=1"
  def maybe_serialize(type, false), do: "#{type}=0"
  def maybe_serialize("dtmf-tones", value), do: "#{value}"

  def maybe_serialize("sprop-parameter-sets", %{sps: sps, pps: pps}),
    do: "sprop-parameter-sets=#{Base.encode64(sps)},#{Base.encode64(pps)}"

  def maybe_serialize(type, {key, value}), do: "#{type}=#{key}:#{value}"
  def maybe_serialize(type, value), do: "#{type}=#{value}"

  @spec maybe_serialize_hex(String.t(), nil | integer) :: binary
  def maybe_serialize_hex(_type, nil), do: ""

  def maybe_serialize_hex(type, value),
    do: "#{type}=#{Integer.to_string(value, 16) |> String.downcase()}"

  @spec maybe_serialize_list([String.t()] | nil, String.t()) :: String.t()
  def maybe_serialize_list([], _sep), do: ""
  def maybe_serialize_list(nil, _sep), do: ""
  def maybe_serialize_list(list, sep), do: Enum.map_join(list, sep, &"#{&1}")

  defp serialize_ice_options(ice_options) do
    Bunch.listify(ice_options) |> Enum.join(" ")
  end

  defp serialize_fingerprint(fingerprint) do
    case fingerprint do
      {:sha1, value} -> "sha-1 #{value}"
      {:sha224, value} -> "sha-224 #{value}"
      {:sha256, value} -> "sha-256 #{value}"
      {:sha384, value} -> "sha-384 #{value}"
      {:sha512, value} -> "sha-512 #{value}"
    end
  end

  defp serialize_setup(setup) when setup in [:active, :passive, :actpass, :holdconn],
    do: Atom.to_string(setup)
end
