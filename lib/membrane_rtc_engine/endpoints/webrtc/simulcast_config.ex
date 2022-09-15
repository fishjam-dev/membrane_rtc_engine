defmodule Membrane.RTC.Engine.Endpoint.WebRTC.SimulcastConfig do
  @moduledoc """
  Module representing simulcast configuration for WebRTC endpoint.
  """

  alias Membrane.RTC.Engine.Track

  @typedoc """
  Simulcast configuration.

  * `enabled` - whether to accept simulcast tracks or not.
  Setting this to false will result in rejecting all incoming
  simulcast tracks i.e. client will not send them.
  * `initial_target_variant` - function used to determine initial
  target variant this endpoint is willing to receive for given track.
  It is called for each track this endpoint subscribes for.
  If not provided, the highest possible variant will be used.
  """
  @type t() :: %__MODULE__{
          enabled: boolean(),
          initial_target_variant: (Track.t() -> Track.variant() | nil)
        }
  defstruct enabled: false,
            initial_target_variant: &__MODULE__.initial_target_variant/1

  @doc """
  Default implementation of `initial_target_variant` function in `t:t/0`.

  Returns nil, which will result in choosing the highest possible encoding.
  """
  @spec initial_target_variant(Track.t()) :: nil
  def initial_target_variant(_track), do: nil
end
