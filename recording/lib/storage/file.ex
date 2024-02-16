defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.File do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to files locally.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  @impl true
  def get_sink(opts) do
    location = Path.join(opts.path_prefix, opts.filename)
    File.touch!(location)
    %Membrane.File.Sink{location: location}
  end

  @impl true
  def save_object(object, path_prefix, filename) do
    location = Path.join(path_prefix, filename)

    with :ok <- File.touch(location) do
      File.write(location, object)
    end
  end
end
