defmodule ExLibSRTP.Native do
  @moduledoc false

  use Unifex.Loader

  require ExLibSRTP
  require Logger

  alias ExLibSRTP.{MasterKey, Policy}

  @spec marshal_ssrc(ExLibSRTP.Policy.ssrc_pattern_t()) :: {ssrc_type :: 1..3, ExLibSRTP.ssrc_t()}
  def marshal_ssrc(:any_inbound), do: {2, 0}
  def marshal_ssrc(:any_outbound), do: {3, 0}
  def marshal_ssrc(ssrc) when ExLibSRTP.is_ssrc(ssrc), do: {1, ssrc}

  @spec marshal_master_keys(Policy.key_spec_t()) :: {keys :: [binary()], mkis :: [binary()]}
  def marshal_master_keys(key) when is_binary(key) do
    {[key], []}
  end

  def marshal_master_keys(keys) when is_list(keys) do
    keys
    |> Enum.map(fn %MasterKey{key: key, mki: mki} -> {key, mki} end)
    |> Enum.unzip()
  end

  @spec marshal_window_size(:default | pos_integer()) :: 0 | 64..32_768
  def marshal_window_size(:default), do: 0

  def marshal_window_size(window) when window < 64 do
    Logger.warn("ExLibSRTP: Window size to small, setting to 64")
    64
  end

  def marshal_window_size(window) when window > 32_768 do
    Logger.warn("ExLibSRTP: Window size to large, setting to 32768")
    32_768
  end

  def marshal_window_size(window), do: window
end
