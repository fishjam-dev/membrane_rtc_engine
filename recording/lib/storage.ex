defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  alias Membrane.RTC.Engine.Track
  @type config_t :: struct

  @type sink_config :: %{
          track: Track.t(),
          path_prefix: Path.t(),
          filename: String.t()
        }

  @type object_config :: %{
          object: binary(),
          path_prefix: Path.t(),
          filename: String.t()
        }

  @callback get_sink(config :: sink_config()) :: struct()
  @callback save_object(config :: object_config()) :: :ok | {:error, String.t()}
end
