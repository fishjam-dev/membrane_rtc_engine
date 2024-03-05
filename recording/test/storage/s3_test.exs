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

  @storage_config %{credentials: @credentials}

  @tag :tmp_dir
  test "create s3 sink", %{tmp_dir: output_dir} do
    filename = "track_1"

    config = %{
      track: %{},
      path_prefix: output_dir,
      filename: filename
    }

    path = Path.join(output_dir, filename)

    assert %Storage.S3.Sink{credentials: @credentials, path: ^path, chunk_size: _chunk_size} =
             Storage.S3.get_sink(config, @storage_config)
  end

  setup :verify_on_exit!
  setup :set_mox_from_context

  @tag :tmp_dir
  test "save report", %{tmp_dir: output_dir} do
    {:ok, reporter} = Reporter.start(UUID.uuid4())

    filename = "track_1.msr"
    track = create_track(:video)
    offset = 0

    :ok = Reporter.add_track(reporter, track, filename, offset)

    report_json =
      reporter
      |> Reporter.get_report()
      |> Jason.encode!()

    config = %{
      object: report_json,
      path_prefix: output_dir,
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
