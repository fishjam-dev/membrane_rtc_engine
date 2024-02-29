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

  setup :verify_on_exit!
  setup :set_mox_from_context

  test "send empty file" do
    perform_test(3, "test/fixtures/empty.txt")

    # in case of empty file ex_aws_s3 library will send empty chunk
    # bacause it's internal implementaion of ex_aws_s3 library we don't test it
    assert_receive :upload_initialized
    assert_receive :upload_completed
  end

  test "send small file" do
    perform_test(3, "test/fixtures/small.txt")

    assert_receive :upload_initialized
    assert_receive :chunk_uploaded
    assert_receive :upload_completed
  end

  test "send file bigger than chunk size" do
    perform_test(4, "test/fixtures/big.txt")

    assert_receive :upload_initialized
    assert_receive :chunk_uploaded
    assert_receive :chunk_uploaded
    assert_receive :upload_completed
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
    assert_end_of_stream(pipeline, :sink)
  end

  defp setup_mock_http_request(call_no) do
    pid = self()

    expect(ExAws.Request.HttpMock, :request, call_no, fn method, url, _body, _headers, _opts ->
      case %{method: method, url: url} do
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
          url: @url_prefix <> _rest
        } ->
          send(pid, :chunk_uploaded)

          {:ok,
           %{
             status_code: 200,
             headers: %{"ETag" => "1"}
           }}
      end
    end)
  end
end
