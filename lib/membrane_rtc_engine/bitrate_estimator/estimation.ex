defmodule Membrane.WebRTC.BitrateEstimator.Estimation do
  @moduledoc """
  Struct describing bandwidth estimation struct
  """

  @enforce_keys [:estimation, :error]
  defstruct [:estimation, :error]

  @typedoc """
  Type describing statistical parameters of the variable B containing bandwidths used by arrival of every buffer
  - estimation: E{B} - expected value of bitrate used by this sample of buffers
  - error: B_hat - standard deviation of the series
  """
  @type t() :: %__MODULE__{
          estimation: number(),
          error: number
        }
end
