defmodule Membrane.RTC.Engine.Track.BitrateEstimation do
  @moduledoc """
  Struct describing bitrate estimation.
  """

  @enforce_keys [:estimation, :error]
  defstruct [:estimation, :error]

  @typedoc """
  Type describing bitrate estimation:
  - estimation - estimated bitrate in kbps
  - error - expected error of the estimation 
  """
  @type t() :: %__MODULE__{
          estimation: number(),
          error: number
        }
end
