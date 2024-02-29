defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3.Sink do
  @moduledoc """
  Element that saves given stream to S3 bucketin real time.
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
        acc_payload: <<>>,
        curr_index: 1,
        parts: []
      })

    {[], state}
  end

  @impl true
  def handle_setup(_ctx, state) do
    s3_upload = ExAws.S3.upload([], state.credentials.bucket, state.path)
    aws_config = Storage.S3.create_aws_config(state.credentials)
    {:ok, operation} = ExAws.S3.Upload.initialize(s3_upload, aws_config)

    {[], %{state | aws_op: operation, aws_config: aws_config}}
  end

  @impl true
  def handle_buffer(:input, %Membrane.Buffer{payload: payload}, _ctx, state) do
    accumulated_payload = payload <> state.acc_payload

    if byte_size(accumulated_payload) > state.chunk_size do
      {payload_to_send, remaining_payload} =
        split_payload_over_chunk_size(accumulated_payload, state.chunk_size)

      new_state = handle_upload(state, payload_to_send)

      {[], new_state |> update_payload(remaining_payload) |> increment_index()}
    else
      {[], update_payload(state, accumulated_payload)}
    end
  end

  @impl true
  def handle_end_of_stream(:input, _ctx, state) do
    state =
      if byte_size(state.acc_payload) > 0,
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

  defp split_payload_over_chunk_size(accumulated_payload, chunk_size) do
    <<payload_to_send::binary-size(chunk_size), remaining_payload::binary>> = accumulated_payload
    {payload_to_send, remaining_payload}
  end

  defp handle_upload(state, payload_to_send) do
    result =
      ExAws.S3.Upload.upload_chunk(
        {payload_to_send, state.curr_index},
        state.aws_op,
        state.aws_config
      )

    part = handle_upload_result(result)
    add_part(state, part)
  end

  defp handle_upload_result({:error, reason}),
    do: raise("S3 recording storage couldn't upload chunk, reason: #{inspect(reason)}")

  defp handle_upload_result(part), do: part

  defp increment_index(state), do: %{state | curr_index: state.curr_index + 1}
  defp update_payload(state, payload), do: %{state | acc_payload: payload}
  defp add_part(state, part), do: %{state | parts: [part | state.parts]}
end
