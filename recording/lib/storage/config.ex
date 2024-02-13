defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.Config do
  @moduledoc """
  Config passed to recording storage
  """

  alias Membrane.RTC.Engine.Track

  @enforce_keys [:track, :output_dir, :filename]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          track: Track.t(),
          output_dir: Path.t(),
          filename: String.t()
        }
end
