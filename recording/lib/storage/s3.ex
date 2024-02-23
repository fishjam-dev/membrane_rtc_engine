defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3 do
  @moduledoc """
  `Membrane.RTC.Engine.Endpoint.Recording.Storage` implementation that saves the stream to the pointed AWS S3 bucket.
  """
  @behaviour Membrane.RTC.Engine.Endpoint.Recording.Storage

  # minimal chunk size based on aws specification (in bytes)
  @chunk_size 5_242_880

  @type credentials_t :: %{
          access_key_id: String.t(),
          secret_access_key: String.t(),
          region: String.t(),
          bucket: String.t()
        }

  @impl true
  def get_sink(opts) do
    path = Path.join(opts.path_prefix, opts.filename)
    credentials = get_credentials()

    %Membrane.RTC.Engine.Endpoint.Recording.S3.Sink{
      path: path,
      credentials: credentials,
      chunk_size: @chunk_size
    }
  end

  @impl true
  def save_object(object, output_dir, filename) do
    path = Path.join(output_dir, filename)
    credentials = get_credentials()
    aws_config = create_aws_config(credentials)

    result =
      credentials.bucket
      |> ExAws.S3.put_object(path, object, [])
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

  defp get_credentials() do
    # change to envs
    %{
      access_key_id: "add",
      secret_access_key: "add",
      region: "add",
      bucket: "add"
    }
  end
end
