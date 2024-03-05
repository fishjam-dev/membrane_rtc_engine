defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3.Sink do
  @moduledoc """
  Element that saves given stream to S3 bucket in real time.
  """

  use Membrane.Sink

  alias Membrane.RTC.Engine.Endpoint.Recording.Storage

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
  def handle_setup(_ctx, state) do
    s3_upload = ExAws.S3.upload([], state.credentials.bucket, state.path)
    aws_config = Storage.S3.create_aws_config(state.credentials)

    case ExAws.S3.Upload.initialize(s3_upload, aws_config) do
      {:ok, operation} ->
        {[], %{state | aws_op: operation, aws_config: aws_config}}

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

      {:ok, response} ->
        raise("S3 recording storage couldn't complete uploading, response: #{inspect(response)}")
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
