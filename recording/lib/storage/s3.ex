defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3 do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to the pointed AWS S3 bucket.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  alias Membrane.RTC.Engine.Endpoint.Recording.Storage

  # minimal chunk size based on aws specification (in bytes)
  @chunk_size 5_242_880

  @type credentials_t :: %{
          access_key_id: String.t(),
          secret_access_key: String.t(),
          region: String.t(),
          bucket: String.t()
        }

  @type storage_opts :: %{credentials: credentials_t()}

  @impl true
  @spec get_sink(Storage.recording_config(), storage_opts()) :: struct()
  def get_sink(config, storage_opts) do
    path = Path.join(config.path_prefix, config.filename)

    %__MODULE__.Sink{
      path: path,
      credentials: storage_opts.credentials,
      chunk_size: @chunk_size
    }
  end

  @impl true
  def save_object(config, storage_opts) do
    path = Path.join(config.path_prefix, config.filename)
    credentials = storage_opts.credentials
    aws_config = create_aws_config(credentials)

    result =
      credentials.bucket
      |> ExAws.S3.put_object(path, config.object, [])
      |> ExAws.request(aws_config)

    case result do
      {:ok, %{status_code: 200}} -> :ok
      {:ok, response} -> {:error, response}
    end
  end

  @spec create_aws_config(credentials_t()) :: list()
  def create_aws_config(credentials) do
    credentials
    |> Enum.reject(fn {key, _value} -> key == :bucket end)
    |> then(&ExAws.Config.new(:s3, &1))
    |> Map.to_list()
  end
end
