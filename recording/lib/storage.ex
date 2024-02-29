defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  alias Membrane.RTC.Engine.Track
  @type config_t :: module()

  @type recording_config :: %{
          track: Track.t(),
          path_prefix: Path.t(),
          filename: String.t()
        }

  @type object_config :: %{
          object: binary(),
          path_prefix: Path.t(),
          filename: String.t()
        }

  @callback get_sink(recording_config :: recording_config(), opts :: any()) :: struct()
  @callback save_object(object_config :: object_config(), opts :: any()) ::
              :ok | {:error, String.t()}
end
