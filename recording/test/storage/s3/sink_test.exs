defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3.SinkTest do
  use ExUnit.Case, async: false

  import Membrane.ChildrenSpec
  import Membrane.Testing.Assertions
  import Mox

  alias Membrane.RTC.Engine.Endpoint.Recording.Storage.S3.Sink

  @chunk_size 5_242_880
  @credentials %{
    access_key_id: "123456789",
    secret_access_key: "987654321",
    region: "eu-central-1",
    bucket: "bucket"
  }
  @path "path"
  @upload_id "upload_id"

  @url_prefix "https://s3.eu-central-1.amazonaws.com/#{@credentials.bucket}/#{@path}?"

  @initialize_url @url_prefix <> "uploads=1"
  @complete_url @url_prefix <> "uploadId=#{@upload_id}"

  @pipeline_timeout 5_000

  setup :verify_on_exit!
  setup :set_mox_from_context

  test "send empty file" do
    perform_test(4, "test/fixtures/empty.txt")

    # in case of empty file ex_aws_s3 library will send empty chunk
    # because it's an internal implementation of the ex_aws_s3 library, we don't test it
    assert_received :upload_initialized
    assert_received :upload_completed
  end

  test "send small file" do
    file_path = "test/fixtures/small.txt"
    perform_test(4, file_path)

    assert_received :upload_initialized
    assert_received {:chunk_uploaded, body}
    assert_received :upload_completed

    assert body == File.read!(file_path)
  end

  test "send file bigger than chunk size" do
    file_path = "test/fixtures/big.txt"
    perform_test(6, file_path)

    assert_received :upload_initialized
    assert_received {:chunk_uploaded, body_1}
    assert_received {:chunk_uploaded, body_2}
    assert_received {:chunk_uploaded, body_3}
    assert_received :upload_completed

    assert body_1 <> body_2 <> body_3 == File.read!(file_path)
  end

  test "cleanup resources on crash" do
    file_path = "test/fixtures/big.txt"
    setup_mock_http_request(3, true)

    sink = %Sink{
      path: @path,
      credentials: @credentials,
      chunk_size: @chunk_size
    }

    spec = [
      child(:source, %Membrane.File.Source{location: file_path, seekable?: true})
      |> child(:sink, sink)
    ]

    {:ok, _supervisor, pipeline} = Membrane.Testing.Pipeline.start(spec: spec)

    # make sure resource guard is defined
    Process.sleep(2_000)

    pid = Membrane.Testing.Pipeline.get_child_pid!(pipeline, :sink)
    Process.exit(pid, :brutal_kill)

    refute_receive {:chunk_uploaded, _body}
    refute_receive :upload_completed

    # resource guard
    assert_receive :get_chunk_list, 2_000
    assert_receive :delete_chunks, 2_000
  end

  defp perform_test(request_no, file_path) do
    setup_mock_http_request(request_no)

    sink = %Sink{
      path: @path,
      credentials: @credentials,
      chunk_size: @chunk_size
    }

    spec = [
      child(:source, %Membrane.File.Source{location: file_path})
      |> child(:sink, sink)
    ]

    pipeline = Membrane.Testing.Pipeline.start_link_supervised!(spec: spec)
    assert_end_of_stream(pipeline, :sink, :input, @pipeline_timeout)
    Membrane.Testing.Pipeline.terminate(pipeline)

    # resource guard
    assert_receive :get_chunk_list, 2_000
    refute_receive :delete_chunks, 2_000
  end

  defp setup_mock_http_request(call_no, get_request \\ false) do
    pid = self()

    expect(ExAws.Request.HttpMock, :request, call_no, fn method, url, body, _headers, _opts ->
      case %{method: method, url: url, body: body} do
        %{
          method: :post,
          url: @complete_url
        } ->
          send(pid, :upload_completed)

          {:ok,
           %{
             status_code: 200,
             body: """
             <CompleteMultipartUploadResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
               <Location>https://s3-eu-west-1.amazonaws.com/#{@credentials.bucket}/#{@path}</Location>
               <Bucket>#{@credentials.bucket}</Bucket>
               <Key>#{@path}</Key>
               <ETag>&quot;17fbc0a106abbb6f381aac6e331f2a19-1&quot;</ETag>
             </CompleteMultipartUploadResult>
             """
           }}

        %{
          method: :post,
          url: @initialize_url
        } ->
          send(pid, :upload_initialized)

          {:ok,
           %{
             status_code: 200,
             body: """
             <InitiateMultipartUploadResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
               <Bucket>#{@credentials.bucket}</Bucket>
               <Key>#{@path}</Key>
               <UploadId>#{@upload_id}</UploadId>
             </InitiateMultipartUploadResult>
             """
           }}

        %{
          method: :put,
          url: @url_prefix <> _rest,
          body: body
        } ->
          send(pid, {:chunk_uploaded, body})
          {:ok, %{status_code: 200, headers: %{"ETag" => "1"}}}

        %{
          method: :get,
          url: @url_prefix <> _rest
        } ->
          send(pid, :get_chunk_list)

          if get_request,
            do: {:ok, %{status_code: 200}},
            else: {:ok, %{status_code: 400, reason: "reason"}}

        %{
          method: :delete,
          url: @url_prefix <> _rest
        } ->
          send(pid, :delete_chunks)
          {:ok, %{status_code: 200}}
      end
    end)
  end
end
