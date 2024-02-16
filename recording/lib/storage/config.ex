defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.Config do
  @moduledoc """
  Config passed to recording storage
  """

  alias Membrane.RTC.Engine.Track

  @enforce_keys [:track, :path_prefix, :filename]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          track: Track.t(),
          path_prefix: Path.t(),
          filename: String.t()
        }
end
