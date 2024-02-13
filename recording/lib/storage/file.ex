defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.File do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to files locally.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  @impl true
  def get_sink(opts) do
    location = Path.join(opts.output_dir, opts.filename)
    File.mkdir_p!(opts.output_dir)
    File.touch!(location)
    %Membrane.File.Sink{location: location}
  end

  @impl true
  def save_object(object, output_dir, filename) do
    location = Path.join(output_dir, filename)

    with :ok <- File.mkdir_p(output_dir),
         :ok <- File.touch(location) do
      File.write(location, object)
    end
  end
end
