defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  @callback get_sink(config :: __MODULE__.Config.t()) :: struct()
  @callback save_object(object :: binary(), output_dir :: Path.t(), filename :: String.t()) ::
              :ok | {:error, String.t()}
end
