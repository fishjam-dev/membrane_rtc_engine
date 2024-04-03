defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3 do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to the pointed AWS S3 bucket.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.Recording.Storage

  # minimal chunk size based on aws specification (in bytes)
  @chunk_size 5_242_880

  @type credentials_t :: %{
          access_key_id: String.t(),
          secret_access_key: String.t(),
          region: String.t(),
          bucket: String.t()
        }

  @type storage_opts :: %{:credentials => credentials_t(), optional(:path_prefix) => Path.t()}

  @impl true
  @spec get_sink(Storage.recording_config(), storage_opts()) :: struct()
  def get_sink(config, storage_opts) do
    path_prefix = Map.get(storage_opts, :path_prefix, "")
    path = Path.join([path_prefix, config.recording_id, config.filename])

    %__MODULE__.Sink{
      path: path,
      credentials: storage_opts.credentials,
      chunk_size: @chunk_size
    }
  end

  @impl true
  def save_object(config, storage_opts) do
    path_prefix = Map.get(storage_opts, :path_prefix, "")
    path = Path.join([path_prefix, config.recording_id, config.filename])
    credentials = storage_opts.credentials
    aws_config = create_aws_config(credentials)

    result =
      credentials.bucket
      |> ExAws.S3.put_object(path, config.object, [])
      |> ExAws.request(aws_config)

    case result do
      {:ok, %{status_code: 200}} ->
        :ok

      {:error, response} ->
        Membrane.Logger.error(
          "Couldn't save object on S3 bucket, recording id: #{config.recording_id}"
        )

        {:error, response}
    end
  end

  @impl true
  def on_close(files, recording_id, storage_opts) do
    case list_objects(recording_id, storage_opts) do
      {:ok, objects} -> fix_objects(files, recording_id, storage_opts, objects)
      {:error, :list_objects} -> :error
    end
  end

  @spec create_aws_config(credentials_t()) :: list()
  def create_aws_config(credentials) do
    credentials
    |> Enum.reject(fn {key, _value} -> key == :bucket end)
    |> then(&ExAws.Config.new(:s3, &1))
    |> Map.to_list()
  end

  defp fix_objects(
         files,
         recording_id,
         storage_opts,
         objects
       ) do
    fixed? =
      Enum.all?(files, fn {filename, {file_path, file_size}} ->
        s3_size_result = Map.fetch(objects, filename)

        correct_object?(s3_size_result, file_size) ||
          fix_object(file_path, filename, recording_id, storage_opts)
      end)

    if fixed? do
      :ok
    else
      objects
      |> Enum.map(fn {filename, _size} -> filename end)
      |> clean_objects(recording_id, storage_opts)

      :error
    end
  end

  defp list_objects(recording_id, storage_opts) do
    path_prefix =
      storage_opts
      |> Map.get(:path_prefix, "")
      |> Path.join(recording_id)

    credentials = storage_opts.credentials
    config = create_aws_config(credentials)

    response =
      credentials.bucket
      |> ExAws.S3.list_objects(prefix: path_prefix)
      |> ExAws.request(config)

    case response do
      {:ok, %{body: %{contents: contents}}} ->
        {:ok, Map.new(contents, &parse_stats/1)}

      _else ->
        Membrane.Logger.error("Couldn't list objects on S3 bucket, recording id: #{recording_id}")
        {:error, :list_objects}
    end
  end

  defp clean_objects(objects, recording_id, %{credentials: credentials}) do
    config = create_aws_config(credentials)

    result =
      credentials.bucket
      |> ExAws.S3.delete_all_objects(objects)
      |> ExAws.request(config)

    case result do
      {:ok, _term} ->
        :ok

      {:error, _reason} ->
        Membrane.Logger.error(
          "Couldn't clean objects on S3 bucket, recording id: #{recording_id}"
        )

        :error
    end
  end

  defp correct_object?({:ok, s3_size}, file_size), do: s3_size >= file_size
  defp correct_object?(:error, _file_size), do: false

  defp fix_object(file_path, filename, recording_id, storage_opts) do
    config =
      file_path
      |> File.read!()
      |> save_object_config(recording_id, filename)

    case save_object(config, storage_opts) do
      :ok -> true
      {:error, _response} -> false
    end
  end

  defp save_object_config(object, recording_id, filename) do
    %{
      object: object,
      recording_id: recording_id,
      filename: filename
    }
  end

  defp parse_stats(stats) do
    filename = stats.key |> String.split("/") |> List.last()
    size = String.to_integer(stats.size)
    {filename, size}
  end
end
