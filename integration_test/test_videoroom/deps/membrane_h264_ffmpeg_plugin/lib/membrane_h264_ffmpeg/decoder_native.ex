defmodule Membrane.H264.FFmpeg.Decoder.Native do
  @moduledoc false
  use Unifex.Loader

  @spec create! :: reference()
  def create!() do
    case create() do
      {:ok, decoder_ref} -> decoder_ref
      {:error, reason} -> raise "Failed to create native decoder: #{inspect(reason)}"
    end
  end
end
