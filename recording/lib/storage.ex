defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  alias Membrane.RTC.Engine.Track

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

  @doc """
  Callback invoked when a new track is added to `RecordingEndpoint`.
  It should return a struct that will be further added as a child to the spec.
  """
  @callback get_sink(recording_config :: recording_config(), opts :: any()) :: struct()

  @doc """
  Callback invoked when `RecordingEndpoint` finishes processing all tracks.
  Object is a report that includes all the information needed for deserializing given tracks.
  """
  @callback save_object(object_config :: object_config(), opts :: any()) ::
              :ok | {:error, String.t()}
end
