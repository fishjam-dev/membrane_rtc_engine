defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3.Sink do
  @moduledoc """
  Element that saves given stream to S3 bucket in real time.
  """

  use Membrane.Sink

  require Membrane.Logger

  alias Membrane.RTC.Engine.Endpoint.Recording.{Metrics, Storage}

  @type credentials :: %{
          access_key_id: String.t(),
          secret_access_key: String.t(),
          region: String.t(),
          bucket: String.t()
        }

  def_input_pad :input, accepted_format: _any

  def_options credentials: [
                spec: credentials(),
                description: """
                Credentials with all information needed to get access to pointed AWS S3 bucket.
                """
              ],
              path: [
                spec: Path.t(),
                description: """
                Path to file in a bucket
                """
              ],
              chunk_size: [
                spec: pos_integer(),
                description: """
                Size of a single chunk that will be sent to s3 bucket
                """
              ]

  @impl true
  def handle_init(_ctx, options) do
    state =
      options
      |> Map.merge(%{
        aws_op: nil,
        aws_config: nil,
        acc_payload: [],
        payload_size: 0,
        curr_index: 1,
        parts: []
      })

    {[], state}
  end

  @impl true
  def handle_setup(ctx, state) do
    s3_upload = ExAws.S3.upload([], state.credentials.bucket, state.path)
    config = Storage.S3.create_aws_config(state.credentials)

    case ExAws.S3.Upload.initialize(s3_upload, config) do
      {:ok, op} ->
        Membrane.ResourceGuard.register(ctx.resource_guard, fn ->
          clean_multipart_upload(op, config)
        end)

        {[], %{state | aws_op: op, aws_config: config}}

      {:error, reason} ->
        raise "S3 upload initialization returned error with reason: #{inspect(reason)}"
    end
  end

  @impl true
  def handle_buffer(:input, %Membrane.Buffer{payload: payload}, _ctx, state) do
    accumulated_payload = [payload | state.acc_payload]
    size = state.payload_size + byte_size(payload)

    if state.payload_size >= state.chunk_size do
      {[], handle_upload(state, accumulated_payload)}
    else
      {[], %{state | acc_payload: accumulated_payload, payload_size: size}}
    end
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    state =
      if state.payload_size > 0,
        do: handle_upload(state, state.acc_payload),
        else: state

    result = ExAws.S3.Upload.complete(Enum.reverse(state.parts), state.aws_op, state.aws_config)

    case result do
      {:ok, %{status_code: 200}} ->
        {[], state}

      {:error, response} ->
        raise("S3 recording storage couldn't complete uploading, response: #{inspect(response)}")
    end
  end

  defp clean_multipart_upload(op, config) do
    result =
      op.bucket
      |> ExAws.S3.list_parts(op.path, op.upload_id)
      |> ExAws.request(config)

    case result do
      {:ok, _response} ->
        Metrics.emit_aborted_upload_event([])

        Membrane.Logger.warning(
          "Recording upload: #{op.path} was not complited. Aborting multipart upload"
        )

        op.bucket
        |> ExAws.S3.abort_multipart_upload(op.path, op.upload_id)
        |> ExAws.request(config)

      # Error means that multipart was completed and we cannot get cached chunks
      {:error, _reponse} ->
        Metrics.emit_completed_upload_event([])
        :ok
    end
  end

  defp handle_upload(state, payload) do
    payload_to_send = serialize_payload(payload)

    result =
      ExAws.S3.Upload.upload_chunk(
        {payload_to_send, state.curr_index},
        state.aws_op,
        state.aws_config
      )

    part = handle_upload_result(result)
    state |> add_part(part) |> increment_index() |> empty_payload()
  end

  defp handle_upload_result({:error, reason}),
    do: raise("S3 recording storage couldn't upload chunk, reason: #{inspect(reason)}")

  defp handle_upload_result(part), do: part

  defp add_part(state, part), do: %{state | parts: [part | state.parts]}
  defp empty_payload(state), do: %{state | acc_payload: [], payload_size: 0}
  defp increment_index(state), do: %{state | curr_index: state.curr_index + 1}

  defp serialize_payload(acc_payload),
    do: acc_payload |> Enum.reverse() |> IO.iodata_to_binary()
end
