defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage do
  @moduledoc """
  An interface for Recording endpoint storage configuration.
  """

  alias Membrane.RTC.Engine.Track

  @type recording_config :: %{
          track: Track.t(),
          recording_id: String.t(),
          filename: String.t()
        }

  @type object_config :: %{
          object: binary(),
          recording_id: String.t(),
          filename: String.t()
        }

  @type files() :: %{(filename :: String.t()) => {Path.t(), size :: pos_integer()}}

  @doc """
  Callback invoked when a new track is added to `RecordingEndpoint`.
  It should return a struct that will be further added as a child to the spec.
  """
  @callback get_sink(recording_config :: recording_config(), opts :: any()) :: struct()

  @doc """
  Callback invoked once all tracks have been processed by `RecordingEndpoint`.
  The returned object can be of two types:

    * `report` - a file containing all the necessary information for deserializing the processed tracks.
    * `recording` - a file that captures a complete track recording.
  """
  @callback save_object(object_config :: object_config(), opts :: any()) ::
              :ok | {:error, String.t()}
  @doc """
  This callback gets invoked exclusively when `FileStorage` is in use and a recording session is being closed.
  Within this callback, you can perform the following actions:

    * Clean up all resources
    * Attempt to rectify any discrepancies between the saved objects and local files

  If error is returned report won't be sent to the storage.
  """
  @callback on_close(file_config :: files(), recording_id :: String.t(), opts :: any()) ::
              :ok | :error
end
