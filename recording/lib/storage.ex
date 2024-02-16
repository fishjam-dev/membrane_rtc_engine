defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  @type config_t :: struct

  @callback get_sink(config :: __MODULE__.Config.t()) :: struct()
  @callback save_object(object :: binary(), path_prefix :: Path.t(), filename :: String.t()) ::
              :ok | {:error, String.t()}
end
