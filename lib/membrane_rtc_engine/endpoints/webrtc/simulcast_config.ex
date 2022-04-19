defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastConfig do
  @moduledoc """
  Module representing simulcast configuration for WebRTC endpoint.
  """

  alias Membrane.RTC.Engine.Track

  @typedoc """
  * `enabled` - whether to accept simulcast tracks or not.
  Setting this to false will result in rejecting all incoming
  simulcast tracks i.e. client will not send them.
  * `default_encoding` - function used to determine initial encoding
  this endpoint is willing to receive for given track.
  It is called for each track this endpoint subscribes for.
  If not provided, the highest possible encoding will be used.
  """
  @type t() :: %__MODULE__{
          enabled: boolean(),
          default_encoding: (Track.t() -> String.t() | nil)
        }
  defstruct enabled: false,
            default_encoding: &__MODULE__.default_encoding/1

  @doc """
  Default implementation of `default_encoding` function in `t:t/0`.

  Returns nil, which will result in choosing the highest possible encoding.
  """
  @spec default_encoding(Track.t()) :: nil
  def default_encoding(_track), do: nil
end
