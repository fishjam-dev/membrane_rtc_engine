defmodule Membrane.RTC.Engine.Endpoint.Recording.Storage.S3Test do
  use ExUnit.Case, async: false

  import MockTrack
  import Mox

  alias Membrane.RTC.Engine.Endpoint.Recording.{Reporter, Storage}

  @credentials %{
    access_key_id: "123456789",
    secret_access_key: "987654321",
    region: "eu-central-1",
    bucket: "bucket"
  }

  @path_prefix "path_prefix"
  @storage_config %{credentials: @credentials, path_prefix: @path_prefix}

  test "create s3 sink" do
    filename = "track_1"
    recording_id = "recording_id"

    config = %{
      track: %{},
      recording_id: recording_id,
      filename: filename
    }

    path = Path.join(recording_id, filename)
    path_with_prefix = Path.join([@path_prefix, recording_id, filename])

    assert %Storage.S3.Sink{
             credentials: @credentials,
             path: ^path_with_prefix,
             chunk_size: _chunk_size
           } = Storage.S3.get_sink(config, @storage_config)

    assert %Storage.S3.Sink{credentials: @credentials, path: ^path, chunk_size: _chunk_size} =
             Storage.S3.get_sink(config, %{credentials: @credentials})
  end

  setup :verify_on_exit!
  setup :set_mox_from_context

  test "save report" do
    {:ok, reporter} = Reporter.start(UUID.uuid4())

    filename = "track_1.msr"
    recording_id = "recording_id"
    track = create_track(:video)
    offset = 0

    :ok = Reporter.add_track(reporter, track, filename, offset)

    report_json =
      reporter
      |> Reporter.get_report()
      |> Jason.encode!()

    config = %{
      object: report_json,
      recording_id: recording_id,
      filename: "report.json"
    }

    setup_mock_http_request(report_json)

    :ok = Storage.S3.save_object(config, @storage_config)
    :ok = Reporter.stop(reporter)
  end

  defp setup_mock_http_request(report) do
    expect(ExAws.Request.HttpMock, :request, 1, fn method, _url, body, _headers, _opts ->
      assert report == body
      assert method == :put

      {:ok, %{status_code: 200}}
    end)
  end
end
