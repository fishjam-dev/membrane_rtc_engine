defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.File do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to files locally.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  @impl true
  def get_sink(config, _storage_opts) do
    location = Path.join(config.path_prefix, config.filename)
    File.touch!(location)
    %Membrane.File.Sink{location: location}
  end

  @impl true
  def save_object(config, _storage_opts) do
    location = Path.join(config.path_prefix, config.filename)

    with :ok <- File.touch(location) do
      File.write(location, config.object)
    end
  end
end
