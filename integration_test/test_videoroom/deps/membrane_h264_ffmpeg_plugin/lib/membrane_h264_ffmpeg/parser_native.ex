defmodule Membrane.H264.FFmpeg.Parser.Native do
  @moduledoc false
  use Unifex.Loader

  @spec get_profile!(reference()) :: Membrane.H264.profile_t()
  def get_profile!(parser_ref) do
    case get_profile(parser_ref) do
      {:error, reason} -> raise "Failed to obtain profile from native parser: #{inspect(reason)}"
      {:ok, profile} -> profile
    end
  end

  @spec create!() :: reference()
  def create!() do
    case create() do
      {:ok, parser_ref} ->
        parser_ref

      {:error, reason} ->
        raise "Failed to create native parser: #{inspect(reason)}"
    end
  end
end
